// Copyright (c) 2025, Peter Ohler, All rights reserved.

package slynk

import (
	"fmt"
	"strings"

	"github.com/ohler55/slip"
)

func init() {
	// Symbol description
	RegisterHandler("slynk:describe-symbol", handleDescribeSymbol)
	RegisterHandler("slynk:describe-function", handleDescribeFunction)
	RegisterHandler("slynk:describe-definition-for-emacs", handleDescribeDefinition)

	// Documentation lookup
	RegisterHandler("slynk:documentation-symbol", handleDocumentationSymbol)

	// Argument lists
	RegisterHandler("slynk:operator-arglist", handleOperatorArglist)
	RegisterHandler("slynk:autodoc", handleAutodoc)
}

// handleDescribeSymbol returns a description of a symbol.
func handleDescribeSymbol(c *Connection, args slip.List) slip.Object {
	if len(args) < 1 {
		return slip.String("No symbol specified")
	}

	name, ok := args[0].(slip.String)
	if !ok {
		return slip.String("Invalid symbol name")
	}

	return slip.String(describeSymbol(c, string(name)))
}

// handleDescribeFunction returns a description of a function.
func handleDescribeFunction(c *Connection, args slip.List) slip.Object {
	if len(args) < 1 {
		return slip.String("No function specified")
	}

	name, ok := args[0].(slip.String)
	if !ok {
		return slip.String("Invalid function name")
	}

	return slip.String(describeSymbol(c, string(name)))
}

// handleDescribeDefinition returns definition info for a symbol.
func handleDescribeDefinition(c *Connection, args slip.List) slip.Object {
	if len(args) < 1 {
		return slip.String("No symbol specified")
	}

	name, ok := args[0].(slip.String)
	if !ok {
		return slip.String("Invalid symbol name")
	}

	return slip.String(describeSymbol(c, string(name)))
}

// describeSymbol builds a description string for a symbol.
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

	var sb strings.Builder

	sb.WriteString(fmt.Sprintf("%s\n", strings.ToUpper(name)))
	sb.WriteString(strings.Repeat("-", len(name)))
	sb.WriteString("\n\n")

	// Check if it's a function
	if fi := pkg.GetFunc(name); fi != nil {
		sb.WriteString("Function:\n")
		if fi.Doc != nil {
			sb.WriteString(fmt.Sprintf("  Arguments: %s\n", formatArglist(fi.Doc.Args)))
			sb.WriteString(fmt.Sprintf("  Returns: %s\n", fi.Doc.Return))
			sb.WriteString("\n")
			sb.WriteString(fi.Doc.Text)
			sb.WriteString("\n")
		}
		return sb.String()
	}

	// Check if it's a variable
	if vv := pkg.GetVarVal(name); vv != nil {
		if vv.Const {
			sb.WriteString("Constant:\n")
		} else {
			sb.WriteString("Variable:\n")
		}
		sb.WriteString(fmt.Sprintf("  Value: %s\n", slip.ObjectString(vv.Val)))
		if vv.Doc != "" {
			sb.WriteString("\n")
			sb.WriteString(vv.Doc)
			sb.WriteString("\n")
		}
		return sb.String()
	}

	return fmt.Sprintf("Symbol %s not found", name)
}

// handleDocumentationSymbol returns the docstring for a symbol.
func handleDocumentationSymbol(c *Connection, args slip.List) slip.Object {
	if len(args) < 1 {
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

// handleOperatorArglist returns the argument list for a function.
func handleOperatorArglist(c *Connection, args slip.List) slip.Object {
	if len(args) < 1 {
		return slip.String("")
	}

	name, ok := args[0].(slip.String)
	if !ok {
		return slip.String("")
	}

	arglist := getArglist(c, string(name))
	if arglist == "" {
		return slip.String("")
	}

	return slip.String(arglist)
}

// handleAutodoc provides inline argument hints.
// Returns: (arglist . highlighted-arg-index) or :not-available
func handleAutodoc(c *Connection, args slip.List) slip.Object {
	if len(args) < 1 {
		return slip.Symbol(":not-available")
	}

	// args[0] is a list of raw forms representing cursor context
	context, ok := args[0].(slip.List)
	if !ok || len(context) == 0 {
		return slip.Symbol(":not-available")
	}

	// Find the function being called (first element of context)
	var funcName string
	switch first := context[0].(type) {
	case slip.Symbol:
		funcName = string(first)
	case slip.String:
		funcName = string(first)
	default:
		return slip.Symbol(":not-available")
	}

	arglist := getArglist(c, funcName)
	if arglist == "" {
		return slip.Symbol(":not-available")
	}

	// Determine which argument we're on (context length - 1)
	argIndex := len(context) - 1
	if argIndex < 0 {
		argIndex = 0
	}

	return slip.List{
		slip.String(fmt.Sprintf("(%s %s)", funcName, arglist)),
		slip.Fixnum(argIndex),
	}
}

// formatArglist formats documentation arguments as a string.
func formatArglist(args []*slip.DocArg) string {
	if len(args) == 0 {
		return ""
	}

	var parts []string
	for _, arg := range args {
		parts = append(parts, arg.Name)
	}
	return strings.Join(parts, " ")
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

	// Check if it's a function
	if fi := pkg.GetFunc(name); fi != nil && fi.Doc != nil {
		return fi.Doc.Text
	}

	// Check if it's a variable
	if vv := pkg.GetVarVal(name); vv != nil {
		return vv.Doc
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

	// Check if it's a function
	if fi := pkg.GetFunc(name); fi != nil && fi.Doc != nil {
		var args []string
		for _, arg := range fi.Doc.Args {
			args = append(args, arg.Name)
		}
		return strings.Join(args, " ")
	}

	return ""
}
