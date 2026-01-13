// Copyright (c) 2025, Peter Ohler, All rights reserved.

package slynk

import (
	"sort"
	"strings"

	"github.com/ohler55/slip"
)

func init() {
	// Flex completion - Slynk's advanced fuzzy completion
	RegisterHandler("slynk:flex-completions", handleFlexCompletions)

	// Standard completions (fallback)
	RegisterHandler("slynk:completions", handleCompletions)
	RegisterHandler("slynk:simple-completions", handleCompletions)

	// Apropos for searching symbols
	RegisterHandler("slynk:apropos-list-for-emacs", handleApropos)
}

// flexMatch represents a completion match with its score.
type flexMatch struct {
	symbol      string
	score       float64
	matchedIdxs []int
	flags       string // Classification flags
}

// handleFlexCompletions implements Slynk's flex completion algorithm.
// Returns: (completions nil) where completions is a list of (symbol score chunks classification)
func handleFlexCompletions(c *Connection, args slip.List) slip.Object {
	if len(args) < 1 {
		return slip.List{slip.List{}, nil}
	}

	pattern, ok := args[0].(slip.String)
	if !ok {
		return slip.List{slip.List{}, nil}
	}

	patternStr := strings.ToLower(string(pattern))
	if patternStr == "" {
		return slip.List{slip.List{}, nil}
	}

	// Get package to search in
	pkgName := "cl-user"
	if len(args) >= 2 {
		if p, ok := args[1].(slip.String); ok {
			pkgName = string(p)
		}
	}

	// Collect matches
	var matches []flexMatch

	// Search in specified package and used packages
	pkg := slip.FindPackage(pkgName)
	if pkg == nil {
		pkg = c.currentPkg
	}

	// Get symbols from package
	collectFlexMatches(&matches, pkg, patternStr)

	// Also search used packages
	for _, usedPkg := range pkg.Uses {
		collectFlexMatches(&matches, usedPkg, patternStr)
	}

	// Also search CL package
	collectFlexMatches(&matches, &slip.CLPkg, patternStr)

	// Sort by score (descending)
	sort.Slice(matches, func(i, j int) bool {
		return matches[i].score > matches[j].score
	})

	// Limit results
	maxResults := 100
	if len(matches) > maxResults {
		matches = matches[:maxResults]
	}

	// Build result list
	completions := make(slip.List, 0, len(matches))
	for _, m := range matches {
		// Build chunk representation for highlighting
		chunks := buildChunks(m.symbol, m.matchedIdxs)

		// Use Fixnum for score (scaled to integer)
		scoreInt := slip.Fixnum(int64(m.score * 10))

		completions = append(completions, slip.List{
			slip.String(m.symbol),
			scoreInt,
			chunks,
			slip.String(m.flags),
		})
	}

	return slip.List{completions, nil}
}

// collectFlexMatches finds flex matches in a package.
func collectFlexMatches(matches *[]flexMatch, pkg *slip.Package, pattern string) {
	if pkg == nil {
		return
	}

	// Check functions
	pkg.EachFuncName(func(name string) {
		if score, idxs := flexScore(strings.ToLower(name), pattern); score > 0 {
			*matches = append(*matches, flexMatch{
				symbol:      name,
				score:       score,
				matchedIdxs: idxs,
				flags:       "f", // function
			})
		}
	})

	// Check variables
	pkg.EachVarName(func(name string) {
		if score, idxs := flexScore(strings.ToLower(name), pattern); score > 0 {
			*matches = append(*matches, flexMatch{
				symbol:      name,
				score:       score,
				matchedIdxs: idxs,
				flags:       "v", // variable
			})
		}
	})
}

// flexScore computes a flex match score and returns matched indices.
// Higher score = better match. Returns 0 if no match.
func flexScore(name, pattern string) (float64, []int) {
	if len(pattern) == 0 {
		return 0, nil
	}
	if len(pattern) > len(name) {
		return 0, nil
	}

	var matchedIdxs []int
	patternIdx := 0
	lastMatchIdx := -1
	score := 0.0
	consecutiveBonus := 0.0

	for i := 0; i < len(name) && patternIdx < len(pattern); i++ {
		if name[i] == pattern[patternIdx] {
			matchedIdxs = append(matchedIdxs, i)

			// Base score for match
			matchScore := 1.0

			// Bonus for matching at start
			if i == 0 {
				matchScore += 10.0
			}

			// Bonus for matching after word boundary (- or :)
			if i > 0 && (name[i-1] == '-' || name[i-1] == ':') {
				matchScore += 5.0
			}

			// Bonus for consecutive matches
			if lastMatchIdx >= 0 && i == lastMatchIdx+1 {
				consecutiveBonus += 2.0
				matchScore += consecutiveBonus
			} else {
				consecutiveBonus = 0.0
			}

			// Penalty for later positions (prefer earlier matches)
			matchScore -= float64(i) * 0.1

			score += matchScore
			lastMatchIdx = i
			patternIdx++
		}
	}

	// Must match all pattern characters
	if patternIdx < len(pattern) {
		return 0, nil
	}

	// Bonus for exact match
	if len(name) == len(pattern) {
		score += 50.0
	}

	// Bonus for prefix match
	if strings.HasPrefix(name, pattern) {
		score += 30.0
	}

	// Normalize score by pattern length
	score = score / float64(len(pattern)) * 10.0

	return score, matchedIdxs
}

// buildChunks creates chunk representation for highlighted display.
func buildChunks(symbol string, matchedIdxs []int) slip.List {
	if len(matchedIdxs) == 0 {
		return slip.List{slip.List{slip.String(symbol), slip.TrueSymbol}}
	}

	chunks := slip.List{}
	matchSet := make(map[int]bool)
	for _, idx := range matchedIdxs {
		matchSet[idx] = true
	}

	start := 0
	inMatch := false
	for i := 0; i <= len(symbol); i++ {
		isMatch := matchSet[i]
		if i == len(symbol) || isMatch != inMatch {
			if i > start {
				text := symbol[start:i]
				if inMatch {
					chunks = append(chunks, slip.List{slip.String(text), nil})
				} else {
					chunks = append(chunks, slip.List{slip.String(text), slip.TrueSymbol})
				}
			}
			start = i
			inMatch = isMatch
		}
	}

	return chunks
}

// handleCompletions implements standard prefix completion.
func handleCompletions(c *Connection, args slip.List) slip.Object {
	if len(args) < 1 {
		return slip.List{slip.List{}, nil}
	}

	prefix, ok := args[0].(slip.String)
	if !ok {
		return slip.List{slip.List{}, nil}
	}

	prefixStr := strings.ToLower(string(prefix))

	// Get package
	pkgName := "cl-user"
	if len(args) >= 2 {
		if p, ok := args[1].(slip.String); ok {
			pkgName = string(p)
		}
	}

	pkg := slip.FindPackage(pkgName)
	if pkg == nil {
		pkg = c.currentPkg
	}

	seen := make(map[string]bool)
	var completions slip.List

	addIfMatches := func(name string) {
		if strings.HasPrefix(strings.ToLower(name), prefixStr) && !seen[name] {
			seen[name] = true
			completions = append(completions, slip.String(name))
		}
	}

	// Collect prefix matches
	pkg.EachFuncName(addIfMatches)
	pkg.EachVarName(addIfMatches)

	for _, usedPkg := range pkg.Uses {
		usedPkg.EachFuncName(addIfMatches)
		usedPkg.EachVarName(addIfMatches)
	}

	slip.CLPkg.EachFuncName(addIfMatches)
	slip.CLPkg.EachVarName(addIfMatches)

	return slip.List{completions, nil}
}

// handleApropos searches for symbols matching a pattern.
func handleApropos(c *Connection, args slip.List) slip.Object {
	if len(args) < 1 {
		return slip.List{}
	}

	pattern, ok := args[0].(slip.String)
	if !ok {
		return slip.List{}
	}

	patternStr := strings.ToLower(string(pattern))

	var results slip.List

	// Search all packages
	for _, pkg := range slip.AllPackages() {
		pkg.EachFuncName(func(name string) {
			if strings.Contains(strings.ToLower(name), patternStr) {
				entry := slip.List{
					slip.Symbol(":designator"), slip.String(pkg.Name + ":" + name),
					slip.Symbol(":function"), slip.String(name),
				}
				results = append(results, entry)
			}
		})
		pkg.EachVarName(func(name string) {
			if strings.Contains(strings.ToLower(name), patternStr) {
				entry := slip.List{
					slip.Symbol(":designator"), slip.String(pkg.Name + ":" + name),
					slip.Symbol(":variable"), slip.String(name),
				}
				results = append(results, entry)
			}
		})
	}

	return results
}
