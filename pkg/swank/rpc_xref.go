// Copyright (c) 2025, Peter Ohler, All rights reserved.

package swank

import (
	"strings"

	"github.com/ohler55/slip"
)

func init() {
	RegisterHandler("swank:xref", handleXref)
	RegisterHandler("swank:xrefs", handleXrefs)
}

// xrefType represents the type of cross-reference query.
type xrefType string

const (
	xrefCalls        xrefType = ":calls"        // what does this function call
	xrefCallers      xrefType = ":callers"      // who calls this function
	xrefReferences   xrefType = ":references"   // who references this variable
	xrefBinds        xrefType = ":binds"        // who binds this variable
	xrefSets         xrefType = ":sets"         // who sets this variable
	xrefMacroexpands xrefType = ":macroexpands" // who expands this macro
	xrefSpecializes  xrefType = ":specializes"  // who specializes this class
)

// handleXref finds cross-references for a symbol.
// Args: (type name) where type is :calls, :callers, :references, etc.
// Response format: ((designator location) ...)
func handleXref(c *Connection, args slip.List) slip.Object {
	if len(args) < 2 {
		return slip.List{}
	}

	// Get xref type
	var refType xrefType
	switch v := args[0].(type) {
	case slip.Symbol:
		refType = xrefType(strings.ToLower(string(v)))
	case slip.String:
		refType = xrefType(strings.ToLower(string(v)))
	default:
		return slip.List{}
	}

	// Get symbol name
	name := ""
	switch v := args[1].(type) {
	case slip.Symbol:
		name = strings.ToLower(string(v))
	case slip.String:
		name = strings.ToLower(string(v))
	default:
		return slip.List{}
	}

	switch refType {
	case xrefCalls:
		return findCalls(c, name)
	case xrefCallers:
		return findCallers(c, name)
	case xrefReferences, xrefBinds, xrefSets:
		return findVariableRefs(c, name, refType)
	case xrefMacroexpands:
		return findMacroExpanders(c, name)
	case xrefSpecializes:
		return findSpecializers(c, name)
	default:
		return slip.List{}
	}
}

// handleXrefs finds multiple cross-references.
// Args: ((type name) ...)
// Response: ((type ((designator location) ...)) ...)
func handleXrefs(c *Connection, args slip.List) slip.Object {
	if len(args) == 0 {
		return slip.List{}
	}

	// args[0] should be a list of (type name) pairs
	queries, ok := args[0].(slip.List)
	if !ok {
		return slip.List{}
	}

	var results slip.List
	for _, q := range queries {
		query, ok := q.(slip.List)
		if !ok || len(query) < 2 {
			continue
		}

		refs := handleXref(c, query)
		refType := query[0]
		results = append(results, slip.List{refType, refs})
	}

	return results
}

// findCalls finds what functions are called by the given function.
// Returns: ((designator location) ...)
func findCalls(c *Connection, funcName string) slip.List {
	var results slip.List

	// Find the function
	fi := findFunction(c, funcName)
	if fi == nil {
		return results
	}

	// Get the lambda body if available
	lam := getLambda(fi)
	if lam == nil {
		return results
	}

	// Track seen symbols to avoid duplicates
	seen := make(map[string]bool)

	// Walk the forms looking for function calls
	for _, form := range lam.Forms {
		walkForCalls(form, seen, &results)
	}

	return results
}

// findCallers finds what functions call the given function.
// Returns: ((designator location) ...)
func findCallers(c *Connection, funcName string) slip.List {
	var results slip.List

	// Search all packages
	for _, pkg := range slip.AllPackages() {
		pkg.EachFuncInfo(func(fi *slip.FuncInfo) {
			lam := getLambda(fi)
			if lam == nil {
				return
			}

			// Check if this function calls funcName
			for _, form := range lam.Forms {
				if callsFunction(form, funcName) {
					loc := makeLocation(fi)
					results = append(results, slip.List{
						slip.String(fi.Pkg.Name + ":" + fi.Name),
						loc,
					})
					return // Only add once per function
				}
			}
		})
	}

	return results
}

// findVariableRefs finds references to a variable.
func findVariableRefs(c *Connection, varName string, refType xrefType) slip.List {
	var results slip.List

	// Search all packages for functions that reference the variable
	for _, pkg := range slip.AllPackages() {
		pkg.EachFuncInfo(func(fi *slip.FuncInfo) {
			lam := getLambda(fi)
			if lam == nil {
				return
			}

			// Check if this function references varName
			for _, form := range lam.Forms {
				if referencesSymbol(form, varName) {
					loc := makeLocation(fi)
					results = append(results, slip.List{
						slip.String(fi.Pkg.Name + ":" + fi.Name),
						loc,
					})
					return
				}
			}
		})
	}

	return results
}

// findMacroExpanders finds functions that use the given macro.
func findMacroExpanders(c *Connection, macroName string) slip.List {
	// Same as callers for macros
	return findCallers(c, macroName)
}

// findSpecializers finds methods that specialize on a class.
func findSpecializers(c *Connection, className string) slip.List {
	// SLIP doesn't have full CLOS introspection for this yet
	return slip.List{}
}

// findFunction finds a function by name in current package and used packages.
func findFunction(c *Connection, name string) *slip.FuncInfo {
	// Check for package prefix
	if idx := strings.Index(name, ":"); idx != -1 {
		pkgName := name[:idx]
		funcName := name[idx+1:]
		if pkg := slip.FindPackage(pkgName); pkg != nil {
			return pkg.GetFunc(funcName)
		}
		return nil
	}

	// Check current package
	if fi := c.currentPkg.GetFunc(name); fi != nil {
		return fi
	}

	// Check used packages
	for _, pkg := range c.currentPkg.Uses {
		if fi := pkg.GetFunc(name); fi != nil {
			return fi
		}
	}

	return nil
}

// getLambda extracts the Lambda from a FuncInfo if it's a user-defined function.
func getLambda(fi *slip.FuncInfo) *slip.Lambda {
	if fi == nil || fi.Create == nil {
		return nil
	}

	// Create a function instance and get its caller
	defer func() { _ = recover() }() // Handle any panics from Create

	fun := fi.Create(nil)
	if funky, ok := fun.(slip.Funky); ok {
		if lam, ok := funky.Caller().(*slip.Lambda); ok {
			return lam
		}
	}
	return nil
}

// walkForCalls walks a form looking for function calls and adds them to results.
func walkForCalls(form slip.Object, seen map[string]bool, results *slip.List) {
	switch v := form.(type) {
	case slip.List:
		if len(v) == 0 {
			return
		}
		// First element might be a function call
		if sym, ok := v[0].(slip.Symbol); ok {
			name := strings.ToLower(string(sym))
			if !seen[name] && !isSpecialForm(name) {
				seen[name] = true
				*results = append(*results, slip.List{
					slip.String(name),
					slip.List{slip.Symbol(":error"), slip.String("No source location")},
				})
			}
		}
		// Recurse into subforms
		for _, sub := range v {
			walkForCalls(sub, seen, results)
		}
	case slip.Funky:
		// Check args of compiled forms
		for _, arg := range v.GetArgs() {
			walkForCalls(arg, seen, results)
		}
	}
}

// callsFunction checks if a form calls the given function.
func callsFunction(form slip.Object, funcName string) bool {
	switch v := form.(type) {
	case slip.List:
		if len(v) == 0 {
			return false
		}
		// Check if this is a call to funcName
		if sym, ok := v[0].(slip.Symbol); ok {
			if strings.ToLower(string(sym)) == funcName {
				return true
			}
		}
		// Recurse into subforms
		for _, sub := range v {
			if callsFunction(sub, funcName) {
				return true
			}
		}
	case slip.Funky:
		// Check if this is the function
		if strings.ToLower(v.GetName()) == funcName {
			return true
		}
		// Check args
		for _, arg := range v.GetArgs() {
			if callsFunction(arg, funcName) {
				return true
			}
		}
	case slip.Symbol:
		// Direct symbol reference (e.g., in funcall)
		if strings.ToLower(string(v)) == funcName {
			return true
		}
	}
	return false
}

// referencesSymbol checks if a form references the given symbol.
func referencesSymbol(form slip.Object, symName string) bool {
	switch v := form.(type) {
	case slip.Symbol:
		return strings.ToLower(string(v)) == symName
	case slip.List:
		for _, sub := range v {
			if referencesSymbol(sub, symName) {
				return true
			}
		}
	case slip.Funky:
		for _, arg := range v.GetArgs() {
			if referencesSymbol(arg, symName) {
				return true
			}
		}
	}
	return false
}

// makeLocation creates a SLIME location for a function.
// Since SLIP doesn't track source locations, we return a descriptive error location.
func makeLocation(fi *slip.FuncInfo) slip.Object {
	return slip.List{
		slip.Symbol(":error"),
		slip.String("No source location available"),
	}
}

// isSpecialForm returns true if the name is a special form that shouldn't be listed as a call.
func isSpecialForm(name string) bool {
	specials := map[string]bool{
		"if": true, "let": true, "let*": true, "progn": true,
		"lambda": true, "quote": true, "setq": true, "setf": true,
		"cond": true, "case": true, "when": true, "unless": true,
		"block": true, "return": true, "return-from": true,
		"tagbody": true, "go": true, "throw": true, "catch": true,
		"unwind-protect": true, "multiple-value-bind": true,
		"multiple-value-call": true, "multiple-value-prog1": true,
		"flet": true, "labels": true, "macrolet": true,
		"the": true, "declare": true, "locally": true,
	}
	return specials[name]
}
