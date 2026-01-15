// Copyright (c) 2025, Peter Ohler, All rights reserved.

package alive

import (
	"sort"
	"strings"

	"github.com/ohler55/slip"
)

// LSP CompletionItemKind values
const (
	CompletionKindText          = 1
	CompletionKindMethod        = 2
	CompletionKindFunction      = 3
	CompletionKindConstructor   = 4
	CompletionKindField         = 5
	CompletionKindVariable      = 6
	CompletionKindClass         = 7
	CompletionKindInterface     = 8
	CompletionKindModule        = 9
	CompletionKindProperty      = 10
	CompletionKindKeyword       = 14
	CompletionKindSnippet       = 15
	CompletionKindConstant      = 21
	CompletionKindTypeParameter = 25
)

func init() {
	RegisterHandler("textDocument/completion", handleCompletion)
}

// handleCompletion handles the textDocument/completion request.
// Returns CompletionList with matching symbols.
func handleCompletion(c *Connection, params interface{}) (interface{}, error) {
	p, ok := params.(map[string]interface{})
	if !ok {
		return emptyCompletionList(), nil
	}

	// Extract the text and position
	// LSP sends: {textDocument: {uri}, position: {line, character}}
	// We need the actual text from context or partial word

	// For now, look for a "context" or extract from position
	// Alive extension may send additional context
	var prefix string

	if context, ok := p["context"].(map[string]interface{}); ok {
		if trigger, ok := context["triggerCharacter"].(string); ok {
			prefix = trigger
		}
	}

	// Check for partialResultToken which might contain the word
	if word, ok := p["word"].(string); ok {
		prefix = word
	}

	// If we have position info, we can't easily get the word without document tracking
	// For MVP, return all symbols if no prefix
	completions := findCompletions(c, strings.ToLower(prefix))

	items := make([]map[string]interface{}, 0, len(completions))
	for _, comp := range completions {
		item := map[string]interface{}{
			"label": comp.name,
			"kind":  comp.kind,
		}
		if comp.detail != "" {
			item["detail"] = comp.detail
		}
		if comp.doc != "" {
			item["documentation"] = map[string]interface{}{
				"kind":  "markdown",
				"value": comp.doc,
			}
		}
		items = append(items, item)
	}

	return map[string]interface{}{
		"isIncomplete": false,
		"items":        items,
	}, nil
}

type completionItem struct {
	name   string
	kind   int
	detail string
	doc    string
}

func emptyCompletionList() map[string]interface{} {
	return map[string]interface{}{
		"isIncomplete": false,
		"items":        []interface{}{},
	}
}

// findCompletions finds all symbols matching the prefix.
// Note: We collect names first, then lookup details separately to avoid
// mutex deadlock (EachFuncName holds lock, GetFunc also needs lock).
func findCompletions(c *Connection, prefix string) []completionItem {
	seen := make(map[string]bool)
	var completions []completionItem

	// Helper to collect function names from a package
	collectFuncNames := func(pkg *slip.Package) []string {
		var names []string
		pkg.EachFuncName(func(name string) {
			names = append(names, name)
		})
		return names
	}

	// Helper to collect variable names from a package
	collectVarNames := func(pkg *slip.Package) []string {
		var names []string
		pkg.EachVarName(func(name string) {
			names = append(names, name)
		})
		return names
	}

	// Add function completions (call GetFunc outside the EachFuncName loop)
	addFuncs := func(names []string, pkg *slip.Package, pkgPrefix string) {
		for _, name := range names {
			displayName := name
			if pkgPrefix != "" {
				displayName = pkgPrefix + name
			}
			if prefix != "" && !strings.HasPrefix(strings.ToLower(displayName), prefix) {
				continue
			}
			if seen[displayName] {
				continue
			}
			seen[displayName] = true

			item := completionItem{
				name: displayName,
				kind: CompletionKindFunction,
			}

			// Now safe to call GetFunc (outside EachFuncName callback)
			if fi := pkg.GetFunc(strings.ToLower(name)); fi != nil && fi.Doc != nil {
				var args []string
				for _, arg := range fi.Doc.Args {
					args = append(args, arg.Name)
				}
				item.detail = "(" + strings.Join(args, " ") + ")"
				item.doc = fi.Doc.Text
			}

			completions = append(completions, item)
		}
	}

	// Add variable completions (call GetVarVal outside the EachVarName loop)
	addVars := func(names []string, pkg *slip.Package) {
		for _, name := range names {
			if prefix != "" && !strings.HasPrefix(strings.ToLower(name), prefix) {
				continue
			}
			if seen[name] {
				continue
			}
			seen[name] = true

			item := completionItem{
				name: name,
				kind: CompletionKindVariable,
			}

			// Now safe to call GetVarVal (outside EachVarName callback)
			if vv := pkg.GetVarVal(strings.ToLower(name)); vv != nil {
				if vv.Const {
					item.kind = CompletionKindConstant
				}
				item.doc = vv.Doc
			}

			completions = append(completions, item)
		}
	}

	// Search current package first
	if c.currentPkg != nil {
		funcNames := collectFuncNames(c.currentPkg)
		addFuncs(funcNames, c.currentPkg, "")

		varNames := collectVarNames(c.currentPkg)
		addVars(varNames, c.currentPkg)

		// Search used packages
		for _, pkg := range c.currentPkg.Uses {
			funcNames := collectFuncNames(pkg)
			addFuncs(funcNames, pkg, "")

			varNames := collectVarNames(pkg)
			addVars(varNames, pkg)
		}
	}

	// Search all packages with package prefix
	for _, pkg := range slip.AllPackages() {
		if pkg == c.currentPkg {
			continue
		}
		pkgPrefix := strings.ToLower(pkg.Name) + ":"
		if prefix != "" && strings.HasPrefix(prefix, pkgPrefix) {
			// User is typing a qualified name
			funcNames := collectFuncNames(pkg)
			addFuncs(funcNames, pkg, pkg.Name+":")
		}
	}

	// Sort by name
	sort.Slice(completions, func(i, j int) bool {
		return completions[i].name < completions[j].name
	})

	return completions
}
