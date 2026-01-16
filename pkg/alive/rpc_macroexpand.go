// Copyright (c) 2025, Peter Ohler, All rights reserved.

package alive

import (
	"fmt"
	"strings"

	"github.com/ohler55/slip"
)

func init() {
	RegisterHandler("$/alive/macroexpand", handleMacroexpand)
	RegisterHandler("$/alive/macroexpand1", handleMacroexpand1)
}

// handleMacroexpand handles the $/alive/macroexpand request.
// Fully expands macros until the result is no longer a macro call.
// Params: {"text": "<lisp code>", "package": "<optional package name>"}
// Returns: {"text": "<expanded form>"}
func handleMacroexpand(c *Connection, params any) (any, error) {
	return macroexpandImpl(c, params, false)
}

// handleMacroexpand1 handles the $/alive/macroexpand1 request.
// Expands macros one level only.
// Params: {"text": "<lisp code>", "package": "<optional package name>"}
// Returns: {"text": "<expanded form>"}
func handleMacroexpand1(c *Connection, params any) (any, error) {
	return macroexpandImpl(c, params, true)
}

// macroexpandImpl is the shared implementation for macroexpand and macroexpand-1.
func macroexpandImpl(c *Connection, params any, once bool) (any, error) {
	p, ok := params.(map[string]any)
	if !ok {
		return nil, fmt.Errorf("invalid params")
	}

	text, ok := p["text"].(string)
	if !ok || text == "" {
		return nil, fmt.Errorf("missing text parameter")
	}

	// Optional package parameter
	pkg := c.currentPkg
	if pkgName, ok := p["package"].(string); ok && pkgName != "" {
		if foundPkg := slip.FindPackage(pkgName); foundPkg != nil {
			pkg = foundPkg
		}
	}

	expanded, err := expandMacro(c.scope, pkg, text, once)
	if err != nil {
		return map[string]any{
			"text": fmt.Sprintf("Error: %s", err),
		}, nil
	}

	return map[string]any{
		"text": slip.ObjectString(expanded),
	}, nil
}

// expandMacro parses and expands a form.
func expandMacro(scope *slip.Scope, pkg *slip.Package, source string, once bool) (slip.Object, error) {
	// Parse the source
	code := slip.Read([]byte(source), scope)
	if len(code) == 0 {
		return nil, nil
	}

	form := code[0]

	// Expand
	if once {
		return macroexpand1(scope, pkg, form)
	}
	return macroexpandFull(scope, pkg, form)
}

// macroexpand1 expands a macro form one level.
func macroexpand1(scope *slip.Scope, pkg *slip.Package, form slip.Object) (slip.Object, error) {
	list, ok := form.(slip.List)
	if !ok || len(list) == 0 {
		// Not a list or empty, nothing to expand
		return form, nil
	}

	// Get the operator
	sym, ok := list[0].(slip.Symbol)
	if !ok {
		// First element is not a symbol, nothing to expand
		return form, nil
	}

	// Look up the function
	fi := findMacro(pkg, string(sym))
	if fi == nil {
		// Not a macro
		return form, nil
	}

	// It's a macro! Expand it by calling with unevaluated args
	args := list[1:]

	// Create the macro call and evaluate it to get the expansion
	expanded := callMacro(scope, fi, args)

	return expanded, nil
}

// macroexpandFull repeatedly expands until the result is not a macro call.
func macroexpandFull(scope *slip.Scope, pkg *slip.Package, form slip.Object) (slip.Object, error) {
	for {
		expanded, err := macroexpand1(scope, pkg, form)
		if err != nil {
			return nil, err
		}

		// Check if we expanded anything
		if expanded == form || slip.ObjectString(expanded) == slip.ObjectString(form) {
			return form, nil
		}

		form = expanded

		// Check if the result is still a macro call
		list, ok := form.(slip.List)
		if !ok || len(list) == 0 {
			return form, nil
		}

		sym, ok := list[0].(slip.Symbol)
		if !ok {
			return form, nil
		}

		fi := findMacro(pkg, string(sym))
		if fi == nil {
			// No longer a macro call
			return form, nil
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
			// On panic, return nil (caller will return original form)
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
