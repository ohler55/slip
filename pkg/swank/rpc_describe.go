// Copyright (c) 2025, Peter Ohler, All rights reserved.

package swank

import (
	"strings"

	"github.com/ohler55/slip"
)

func init() {
	RegisterHandler("swank:describe-symbol", handleDescribeSymbol)
	RegisterHandler("swank:describe-function", handleDescribeFunction)
	RegisterHandler("swank:describe-definition-for-emacs", handleDescribeDefinition)
	RegisterHandler("swank:documentation-symbol", handleDocumentationSymbol)
	RegisterHandler("swank:operator-arglist", handleOperatorArglist)
	RegisterHandler("swank:autodoc", handleAutodoc)
	RegisterHandler("swank:arglist-for-echo-area", handleArglistForEchoArea)
}

// handleDescribeSymbol returns a description of a symbol.
func handleDescribeSymbol(c *Connection, args slip.List) slip.Object {
	if len(args) == 0 {
		return slip.String("")
	}

	name, ok := args[0].(slip.String)
	if !ok {
		return slip.String("")
	}

	return slip.String(describeSymbol(c, string(name)))
}

// handleDescribeFunction returns a description of a function.
func handleDescribeFunction(c *Connection, args slip.List) slip.Object {
	if len(args) == 0 {
		return slip.String("")
	}

	name, ok := args[0].(slip.String)
	if !ok {
		return slip.String("")
	}

	return slip.String(describeSymbol(c, string(name)))
}

// handleDescribeDefinition returns a description for a specific definition type.
func handleDescribeDefinition(c *Connection, args slip.List) slip.Object {
	if len(args) == 0 {
		return slip.String("")
	}

	name, ok := args[0].(slip.String)
	if !ok {
		return slip.String("")
	}

	return slip.String(describeSymbol(c, string(name)))
}

// handleDocumentationSymbol returns documentation for a symbol.
func handleDocumentationSymbol(c *Connection, args slip.List) slip.Object {
	if len(args) == 0 {
		return nil
	}

	name, ok := args[0].(slip.String)
	if !ok {
		return nil
	}

	doc := getDocumentation(c, string(name))
	if doc == "" {
		return nil
	}
	return slip.String(doc)
}

// handleOperatorArglist returns the argument list for an operator.
// Response format: "arglist-string" or nil
func handleOperatorArglist(c *Connection, args slip.List) slip.Object {
	if len(args) == 0 {
		return nil
	}

	name, ok := args[0].(slip.String)
	if !ok {
		return nil
	}

	arglist := getArglist(c, string(name))
	if arglist == "" {
		return nil
	}
	return slip.String(arglist)
}

// handleAutodoc returns argument hints for the function at point.
// Response format: (arglist-string nil) or :not-available
func handleAutodoc(c *Connection, args slip.List) slip.Object {
	if len(args) == 0 {
		return slip.Symbol(":not-available")
	}

	// args[0] is the raw form being typed - may be quoted
	rawForm, ok := args[0].(slip.List)
	if !ok {
		// Try to unwrap a quote form by getting its Args
		if funky, isFunky := args[0].(slip.Funky); isFunky {
			qargs := funky.GetArgs()
			if len(qargs) > 0 {
				rawForm, ok = qargs[0].(slip.List)
			}
		}
	}
	if !ok || len(rawForm) == 0 {
		return slip.Symbol(":not-available")
	}

	// Extract the operator name from the form
	var opName string
	switch v := rawForm[0].(type) {
	case slip.String:
		opName = string(v)
	case slip.Symbol:
		opName = string(v)
	default:
		return slip.Symbol(":not-available")
	}

	arglist := getArglist(c, opName)
	if arglist == "" {
		return slip.Symbol(":not-available")
	}

	return slip.List{
		slip.String("(" + opName + " " + arglist + ")"),
		nil,
	}
}

// handleArglistForEchoArea returns arglist for display in echo area.
func handleArglistForEchoArea(c *Connection, args slip.List) slip.Object {
	if len(args) == 0 {
		return nil
	}

	// Extract function names from the arg
	var names []string
	switch v := args[0].(type) {
	case slip.List:
		for _, item := range v {
			if s, ok := item.(slip.String); ok {
				names = append(names, string(s))
			} else if sym, ok := item.(slip.Symbol); ok {
				names = append(names, string(sym))
			}
		}
	case slip.String:
		names = append(names, string(v))
	case slip.Symbol:
		names = append(names, string(v))
	}

	if len(names) == 0 {
		return nil
	}

	arglist := getArglist(c, names[0])
	if arglist == "" {
		return nil
	}

	return slip.String("(" + names[0] + " " + arglist + ")")
}

// describeSymbol returns a full description of a symbol.
func describeSymbol(c *Connection, name string) string {
	name = strings.ToLower(name)

	// Parse package:symbol format
	pkg := c.currentPkg
	if idx := strings.Index(name, ":"); idx != -1 {
		pkgName := name[:idx]
		name = strings.TrimPrefix(name[idx:], ":")
		if p := slip.FindPackage(pkgName); p != nil {
			pkg = p
		}
	}

	// Check if it's a function/macro
	if fi := pkg.GetFunc(name); fi != nil {
		return formatFuncDescription(fi)
	}

	// Search used packages
	for _, usedPkg := range pkg.Uses {
		if fi := usedPkg.GetFunc(name); fi != nil {
			return formatFuncDescription(fi)
		}
	}

	// Check if it's a variable
	if vv := pkg.GetVarVal(name); vv != nil {
		var result strings.Builder
		result.WriteString(strings.ToUpper(name))
		if vv.Const {
			result.WriteString(" names a constant:\n")
		} else {
			result.WriteString(" names a variable:\n")
		}

		result.WriteString("  Value: ")
		result.WriteString(slip.ObjectString(vv.Val))
		result.WriteString("\n")

		if vv.Doc != "" {
			result.WriteString("  Documentation:\n    ")
			result.WriteString(vv.Doc)
			result.WriteString("\n")
		}
		return result.String()
	}

	return "No description available for " + name
}

// formatFuncDescription formats a function/macro description.
func formatFuncDescription(fi *slip.FuncInfo) string {
	var result []byte

	// Header: "NAME names a function/macro:"
	result = append(result, strings.ToUpper(fi.Name)...)
	result = append(result, " names a "...)
	kind := "function"
	switch fi.Kind {
	case slip.MacroSymbol:
		kind = "macro"
	case slip.GenericFunctionSymbol:
		kind = "generic function"
	case slip.MethodSymbol:
		kind = "method"
	}
	result = append(result, kind...)
	result = append(result, ":\n"...)

	// Use FuncDoc.Describe for the rest (with ansi=false to strip markdown)
	if fi.Doc != nil {
		result = fi.Doc.Describe(result, 2, 80, false)
	}

	return string(result)
}

// getDocumentation returns the documentation for a symbol.
func getDocumentation(c *Connection, name string) string {
	name = strings.ToLower(name)

	// Parse package:symbol format
	pkg := c.currentPkg
	if idx := strings.Index(name, ":"); idx != -1 {
		pkgName := name[:idx]
		name = strings.TrimPrefix(name[idx:], ":")
		if p := slip.FindPackage(pkgName); p != nil {
			pkg = p
		}
	}

	// Check if it's a function in current package
	if fi := pkg.GetFunc(name); fi != nil && fi.Doc != nil {
		return string(slip.AppendDoc(nil, fi.Doc.Text, 0, 80, false))
	}

	// Search used packages
	for _, usedPkg := range pkg.Uses {
		if fi := usedPkg.GetFunc(name); fi != nil && fi.Doc != nil {
			return string(slip.AppendDoc(nil, fi.Doc.Text, 0, 80, false))
		}
	}

	// Check if it's a variable
	if vv := pkg.GetVarVal(name); vv != nil {
		return string(slip.AppendDoc(nil, vv.Doc, 0, 80, false))
	}

	return ""
}

// getArglist returns the argument list for a function.
func getArglist(c *Connection, name string) string {
	name = strings.ToLower(name)

	// Parse package:symbol format
	pkg := c.currentPkg
	if idx := strings.Index(name, ":"); idx != -1 {
		pkgName := name[:idx]
		name = strings.TrimPrefix(name[idx:], ":")
		if p := slip.FindPackage(pkgName); p != nil {
			pkg = p
		}
	}

	// Check if it's a function in current package
	if fi := pkg.GetFunc(name); fi != nil && fi.Doc != nil {
		return formatArglist(fi)
	}

	// Search through used packages
	for _, usedPkg := range pkg.Uses {
		if fi := usedPkg.GetFunc(name); fi != nil && fi.Doc != nil {
			return formatArglist(fi)
		}
	}

	return ""
}

// formatArglist formats a function's argument list as a string.
func formatArglist(fi *slip.FuncInfo) string {
	var args []string
	for _, arg := range fi.Doc.Args {
		args = append(args, arg.Name)
	}
	return strings.Join(args, " ")
}
