// Copyright (c) 2025, Peter Ohler, All rights reserved.

package swank

import (
	"fmt"
	"sort"
	"strings"

	"github.com/ohler55/slip"
)

func init() {
	RegisterHandler("swank:completions", handleCompletions)
	RegisterHandler("swank:simple-completions", handleSimpleCompletions)
	RegisterHandler("swank:fuzzy-completions", handleFuzzyCompletions)
	RegisterHandler("swank:apropos-list-for-emacs", handleApropos)
	RegisterHandler("swank:completions-for-keyword", handleCompletionsForKeyword)
}

// handleCompletions returns completions for a prefix.
// Response format: ((completions...) prefix)
func handleCompletions(c *Connection, args slip.List) slip.Object {
	if len(args) == 0 {
		return slip.List{slip.List{}, slip.String("")}
	}

	prefix, ok := args[0].(slip.String)
	if !ok {
		return slip.List{slip.List{}, slip.String("")}
	}

	prefixStr := strings.ToLower(string(prefix))
	completions := findCompletions(c, prefixStr)

	// Convert to list of strings
	result := make(slip.List, len(completions))
	for i, comp := range completions {
		result[i] = slip.String(comp)
	}

	return slip.List{result, prefix}
}

// handleSimpleCompletions is an alias for completions.
func handleSimpleCompletions(c *Connection, args slip.List) slip.Object {
	return handleCompletions(c, args)
}

// handleFuzzyCompletions returns fuzzy completions for a prefix.
// Response format: ((completion-entries...) nil)
// Each entry: (symbol score chunks doc)
func handleFuzzyCompletions(c *Connection, args slip.List) slip.Object {
	if len(args) == 0 {
		return slip.List{slip.List{}, nil}
	}

	prefix, ok := args[0].(slip.String)
	if !ok {
		return slip.List{slip.List{}, nil}
	}

	prefixStr := strings.ToLower(string(prefix))
	completions := findCompletions(c, prefixStr)

	// Build fuzzy completion entries
	result := make(slip.List, len(completions))
	for i, comp := range completions {
		// Each entry: (symbol score chunks flags)
		// Score is a string like "71.33", chunks highlight matched parts
		// Flags: -f--tm-p where f=func, m=macro, t=type, c=class, p=package
		score := fmt.Sprintf("%.2f", float64(100-i))
		flags := getSymbolFlags(c, comp)
		result[i] = slip.List{
			slip.String(comp),
			slip.String(score),
			slip.List{slip.List{slip.Fixnum(0), slip.Fixnum(len(prefixStr))}},
			slip.String(flags),
		}
	}

	return slip.List{result, nil}
}

// getSymbolFlags returns SLIME-style flags for a symbol.
// Format: "-f-ctm-p" where each position indicates:
// 0: b=boundp, 1: f=fboundp, 2: g=generic-function, 3: c=class,
// 4: t=type, 5: m=macro, 6: s=special-variable, 7: p=package
func getSymbolFlags(c *Connection, name string) string {
	flags := []byte("--------")
	name = strings.ToLower(name)

	// Check if it's a package
	if strings.HasSuffix(name, ":") {
		pkgName := strings.TrimSuffix(name, ":")
		if slip.FindPackage(pkgName) != nil {
			flags[7] = 'p'
		}
		return string(flags)
	}

	// Check current package and used packages
	var fi *slip.FuncInfo
	if fi = c.currentPkg.GetFunc(name); fi == nil {
		for _, pkg := range c.currentPkg.Uses {
			if fi = pkg.GetFunc(name); fi != nil {
				break
			}
		}
	}

	if fi != nil {
		// Check if it's a macro or function
		if fi.Kind == slip.MacroSymbol {
			flags[5] = 'm'
		}
		flags[1] = 'f' // fboundp - has function binding
	}

	// Check if it's a type/class
	// TODO: Add class/type detection when available

	return string(flags)
}

// handleApropos searches for symbols matching a pattern.
// Response format: ((designator doc) ...)
func handleApropos(c *Connection, args slip.List) slip.Object {
	if len(args) == 0 {
		return slip.List{}
	}

	pattern, ok := args[0].(slip.String)
	if !ok {
		return slip.List{}
	}

	patternStr := strings.ToLower(string(pattern))
	var matches []aproposResult

	// Search all packages
	for _, pkg := range slip.AllPackages() {
		pkg.EachFuncName(func(name string) {
			if strings.Contains(name, patternStr) {
				matches = append(matches, aproposResult{
					name:    name,
					pkg:     pkg.Name,
					docType: ":function",
				})
			}
		})
		pkg.EachVarName(func(name string) {
			if strings.Contains(name, patternStr) {
				matches = append(matches, aproposResult{
					name:    name,
					pkg:     pkg.Name,
					docType: ":variable",
				})
			}
		})
	}

	// Convert to result format
	result := make(slip.List, len(matches))
	for i, m := range matches {
		designator := m.name
		if m.pkg != c.currentPkg.Name {
			designator = m.pkg + ":" + m.name
		}
		result[i] = slip.List{
			slip.Symbol(m.docType),
			slip.String(designator),
		}
	}

	return result
}

// handleCompletionsForKeyword returns keyword completions.
func handleCompletionsForKeyword(c *Connection, args slip.List) slip.Object {
	if len(args) == 0 {
		return slip.List{slip.List{}, slip.String("")}
	}

	prefix, ok := args[0].(slip.String)
	if !ok {
		return slip.List{slip.List{}, slip.String("")}
	}

	prefixStr := strings.ToLower(string(prefix))
	if !strings.HasPrefix(prefixStr, ":") {
		prefixStr = ":" + prefixStr
	}

	var completions []string

	// Search keyword package
	if kwPkg := slip.FindPackage("keyword"); kwPkg != nil {
		kwPkg.EachVarName(func(name string) {
			kwName := ":" + name
			if strings.HasPrefix(kwName, prefixStr) {
				completions = append(completions, kwName)
			}
		})
	}

	sort.Strings(completions)

	result := make(slip.List, len(completions))
	for i, comp := range completions {
		result[i] = slip.String(comp)
	}

	return slip.List{result, prefix}
}

type aproposResult struct {
	name    string
	pkg     string
	docType string
}

// findCompletions finds all symbols matching the prefix.
func findCompletions(c *Connection, prefix string) []string {
	seen := make(map[string]bool)
	var completions []string

	addIfMatches := func(name string) {
		if strings.HasPrefix(name, prefix) && !seen[name] {
			seen[name] = true
			completions = append(completions, name)
		}
	}

	// Search current package first
	c.currentPkg.EachFuncName(addIfMatches)
	c.currentPkg.EachVarName(addIfMatches)

	// Search used packages
	for _, pkg := range c.currentPkg.Uses {
		pkg.EachFuncName(addIfMatches)
		pkg.EachVarName(addIfMatches)
	}

	// Search all packages with package prefix
	for _, pkg := range slip.AllPackages() {
		if pkg == c.currentPkg {
			continue
		}
		pkgPrefix := pkg.Name + ":"
		if strings.HasPrefix(prefix, pkgPrefix) {
			// User is typing a qualified name
			shortPrefix := strings.TrimPrefix(prefix, pkgPrefix)
			pkg.EachFuncName(func(name string) {
				if strings.HasPrefix(name, shortPrefix) {
					fullName := pkgPrefix + name
					if !seen[fullName] {
						seen[fullName] = true
						completions = append(completions, fullName)
					}
				}
			})
			pkg.EachVarName(func(name string) {
				if strings.HasPrefix(name, shortPrefix) {
					fullName := pkgPrefix + name
					if !seen[fullName] {
						seen[fullName] = true
						completions = append(completions, fullName)
					}
				}
			})
		}
	}

	sort.Strings(completions)
	return completions
}
