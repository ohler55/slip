// Copyright (c) 2025, Peter Ohler, All rights reserved.

package swank

import (
	"strings"

	"github.com/ohler55/slip"
)

func init() {
	// Standard macroexpand functions
	RegisterHandler("swank:swank-macroexpand-1", handleMacroexpand1)
	RegisterHandler("swank:swank-macroexpand", handleMacroexpand)
	RegisterHandler("swank:swank-macroexpand-all", handleMacroexpandAll)

	// Shorthand aliases
	RegisterHandler("swank:swank-expand-1", handleMacroexpand1)
	RegisterHandler("swank:swank-expand", handleMacroexpand)
}

// handleMacroexpand1 expands a macro form one level.
// Args: (string &optional environment)
// Returns: expanded form as string
func handleMacroexpand1(c *Connection, args slip.List) slip.Object {
	if len(args) == 0 {
		return slip.String("")
	}

	source, ok := args[0].(slip.String)
	if !ok {
		return slip.String("")
	}

	expanded := expandMacro(c, string(source), true)
	return slip.String(slip.ObjectString(expanded))
}

// handleMacroexpand fully expands a macro form.
// Args: (string &optional environment)
// Returns: expanded form as string
func handleMacroexpand(c *Connection, args slip.List) slip.Object {
	if len(args) == 0 {
		return slip.String("")
	}

	source, ok := args[0].(slip.String)
	if !ok {
		return slip.String("")
	}

	expanded := expandMacro(c, string(source), false)
	return slip.String(slip.ObjectString(expanded))
}

// handleMacroexpandAll expands all macros in a form, including subforms.
// Args: (string &optional environment)
// Returns: expanded form as string
func handleMacroexpandAll(c *Connection, args slip.List) slip.Object {
	if len(args) == 0 {
		return slip.String("")
	}

	source, ok := args[0].(slip.String)
	if !ok {
		return slip.String("")
	}

	// For now, treat the same as macroexpand
	// A full implementation would recursively expand all subforms
	expanded := expandMacro(c, string(source), false)
	return slip.String(slip.ObjectString(expanded))
}

// expandMacro parses and expands a form.
func expandMacro(c *Connection, source string, once bool) slip.Object {
	defer func() {
		if r := recover(); r != nil {
			// On error, return the original source
		}
	}()

	// Parse the source
	code := slip.Read([]byte(source), c.scope)
	if len(code) == 0 {
		return nil
	}

	form := code[0]

	// Expand
	if once {
		return macroexpand1(c, form)
	}
	return macroexpandFull(c, form)
}

// macroexpand1 expands a macro form one level.
func macroexpand1(c *Connection, form slip.Object) slip.Object {
	list, ok := form.(slip.List)
	if !ok || len(list) == 0 {
		return form
	}

	// Get the operator
	sym, ok := list[0].(slip.Symbol)
	if !ok {
		return form
	}

	// Look up the function
	fi := findMacro(c.currentPkg, string(sym))
	if fi == nil {
		return form
	}

	// Expand the macro
	args := list[1:]
	expanded := callMacro(c.scope, fi, args)
	if expanded == nil {
		return form
	}

	return expanded
}

// macroexpandFull repeatedly expands until the result is not a macro call.
func macroexpandFull(c *Connection, form slip.Object) slip.Object {
	for {
		expanded := macroexpand1(c, form)

		// Check if we expanded anything
		if expanded == form || slip.ObjectString(expanded) == slip.ObjectString(form) {
			return form
		}

		form = expanded

		// Check if the result is still a macro call
		list, ok := form.(slip.List)
		if !ok || len(list) == 0 {
			return form
		}

		sym, ok := list[0].(slip.Symbol)
		if !ok {
			return form
		}

		fi := findMacro(c.currentPkg, string(sym))
		if fi == nil {
			return form
		}
	}
}

// findMacro looks up a function and returns it only if it's a macro.
func findMacro(pkg *slip.Package, name string) *slip.FuncInfo {
	name = strings.ToLower(name)

	// Try current package first
	fi := pkg.GetFunc(name)
	if fi != nil && fi.Kind == slip.MacroSymbol {
		return fi
	}

	// Try with package prefix
	if idx := strings.Index(name, ":"); idx != -1 {
		pkgName := name[:idx]
		symName := strings.TrimPrefix(name[idx:], ":")
		if p := slip.FindPackage(pkgName); p != nil {
			fi = p.GetFunc(symName)
			if fi != nil && fi.Kind == slip.MacroSymbol {
				return fi
			}
		}
	}

	return nil
}

// callMacro calls a macro with unevaluated arguments and returns the expansion.
func callMacro(scope *slip.Scope, fi *slip.FuncInfo, args slip.List) (result slip.Object) {
	defer func() {
		if r := recover(); r != nil {
			result = nil
		}
	}()

	// Create the function
	f := fi.Create(args)

	// Get the Dynamic wrapper and its Lambda
	dyn, ok := f.(*slip.Dynamic)
	if !ok {
		return nil
	}

	lam, ok := dyn.Self.(*slip.Lambda)
	if !ok {
		return nil
	}

	// Create a scope for macro expansion with Macro=false
	// This causes backquote to return the expanded form WITHOUT evaluating it
	ms := scope.NewScope()
	ms.Macro = false

	// Bind the arguments to the lambda parameters
	if lam.Doc != nil {
		for i, arg := range lam.Doc.Args {
			if i < len(args) && !strings.HasPrefix(arg.Name, "&") {
				ms.Let(slip.Symbol(arg.Name), args[i])
			}
		}
	}

	// Call the lambda body to get the expansion
	return lam.BoundCall(ms, 0)
}
