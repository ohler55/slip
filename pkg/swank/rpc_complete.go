// Copyright (c) 2025, Peter Ohler, All rights reserved.

package swank

import (
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
		// Each entry: (symbol score chunks doc)
		// Score is 0-100, chunks highlight matched parts
		score := slip.Fixnum(100 - i) // Simple scoring by position
		result[i] = slip.List{
			slip.String(comp),
			score,
			slip.List{slip.List{slip.Fixnum(0), slip.Fixnum(len(prefixStr))}},
			slip.String(""),
		}
	}

	return slip.List{result, nil}
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
