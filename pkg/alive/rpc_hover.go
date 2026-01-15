// Copyright (c) 2025, Peter Ohler, All rights reserved.

package alive

import (
	"strings"

	"github.com/ohler55/slip"
)

func init() {
	RegisterHandler("textDocument/hover", handleHover)
}

// handleHover handles the textDocument/hover request.
// Returns hover information for the symbol at the given position.
func handleHover(c *Connection, params any) (any, error) {
	p, ok := params.(map[string]any)
	if !ok {
		return nil, nil
	}

	// Extract the word from the request
	// The Alive extension may send the word directly
	var word string
	if w, ok := p["word"].(string); ok {
		word = w
	}

	// If no word provided, we can't show hover
	if word == "" {
		return nil, nil
	}

	description := describeSymbol(c, word)
	if description == "" {
		return nil, nil
	}

	// Return LSP Hover format
	return map[string]any{
		"contents": map[string]any{
			"kind":  "markdown",
			"value": description,
		},
	}, nil
}

// describeSymbol returns a description of a symbol in markdown format.
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

	// If no package, use CL as default
	if pkg == nil {
		pkg = slip.FindPackage("cl")
	}
	if pkg == nil {
		return ""
	}

	var result strings.Builder

	// Check if it's a function
	if fi := pkg.GetFunc(name); fi != nil {
		result.WriteString("**")
		result.WriteString(strings.ToUpper(name))
		result.WriteString("** — function\n\n")

		if fi.Doc != nil {
			if len(fi.Doc.Args) > 0 {
				result.WriteString("```lisp\n(")
				result.WriteString(name)
				for _, arg := range fi.Doc.Args {
					result.WriteString(" ")
					result.WriteString(arg.Name)
				}
				result.WriteString(")\n```\n\n")
			}

			if fi.Doc.Text != "" {
				result.WriteString(fi.Doc.Text)
				result.WriteString("\n")
			}
		}
		return result.String()
	}

	// Check if it's a variable
	if vv := pkg.GetVarVal(name); vv != nil {
		result.WriteString("**")
		result.WriteString(strings.ToUpper(name))
		result.WriteString("**")
		if vv.Const {
			result.WriteString(" — constant\n\n")
		} else {
			result.WriteString(" — variable\n\n")
		}

		result.WriteString("Value: `")
		result.WriteString(slip.ObjectString(vv.Val))
		result.WriteString("`\n\n")

		if vv.Doc != "" {
			result.WriteString(vv.Doc)
			result.WriteString("\n")
		}
		return result.String()
	}

	// Search in all packages if not found in current
	if pkg != c.currentPkg {
		return ""
	}

	for _, p := range slip.AllPackages() {
		if p == pkg {
			continue
		}
		if fi := p.GetFunc(name); fi != nil {
			result.WriteString("**")
			result.WriteString(p.Name)
			result.WriteString(":")
			result.WriteString(strings.ToUpper(name))
			result.WriteString("** — function\n\n")

			if fi.Doc != nil {
				if len(fi.Doc.Args) > 0 {
					result.WriteString("```lisp\n(")
					result.WriteString(p.Name)
					result.WriteString(":")
					result.WriteString(name)
					for _, arg := range fi.Doc.Args {
						result.WriteString(" ")
						result.WriteString(arg.Name)
					}
					result.WriteString(")\n```\n\n")
				}

				if fi.Doc.Text != "" {
					result.WriteString(fi.Doc.Text)
					result.WriteString("\n")
				}
			}
			return result.String()
		}
	}

	return ""
}
