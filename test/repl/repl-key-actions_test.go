// Copyright (c) 2026, Peter Ohler, All rights reserved.

package repl

import (
	"sort"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func keyActionNames(t *testing.T) []string {
	t.Helper()
	var names []string
	(&sliptest.Function{
		Source: `(repl-key-actions)`,
		Validate: func(t *testing.T, v slip.Object) {
			list, ok := v.(slip.List)
			tt.Equal(t, true, ok, "expected a list, got %T", v)
			for _, a := range list {
				sym, ok := a.(slip.Symbol)
				tt.Equal(t, true, ok, "expected a symbol, got %T", a)
				names = append(names, string(sym))
			}
		},
	}).Test(t)
	return names
}

func TestReplKeyActionsSorted(t *testing.T) {
	names := keyActionNames(t)
	tt.Equal(t, true, sort.StringsAreSorted(names), names)
}

func TestReplKeyActionsUnique(t *testing.T) {
	seen := map[string]bool{}
	for _, name := range keyActionNames(t) {
		tt.Equal(t, false, seen[name], "duplicate %s", name)
		seen[name] = true
	}
}

func TestReplKeyActionsContainsSpecNames(t *testing.T) {
	have := map[string]bool{}
	for _, name := range keyActionNames(t) {
		have[name] = true
	}
	for _, name := range []string{
		"line-begin", "line-end", "back-char", "forward-char", "previous-line",
		"next-line", "back-word", "forward-word", "delete-char", "delete-back-char",
		"delete-word", "delete-back-word", "kill-line", "swap-chars", "collapse-space",
		"newline", "newline-after", "enter", "eval-form", "exit", "help", "describe",
		"tab", "shift-tab", "clear-form", "match-open", "match-close", "edit-form",
		"history-back", "history-forward", "search-history-back",
		"search-history-forward", "nth-history", "stash-add", "stash-back",
		"stash-forward", "search-stash-back", "search-stash-forward", "nth-stash",
		"enter-unicode", "reset-term", "copy", "cut", "paste", "form-begin", "form-end",
	} {
		tt.Equal(t, true, have[name], "missing action %s", name)
	}
}

func TestReplKeyActionsExcludesInternal(t *testing.T) {
	for _, name := range keyActionNames(t) {
		for _, internal := range []string{"bad", "add-byte", "top-uni", "add-uni", "esc", "esc5b"} {
			tt.Equal(t, true, name != internal, "internal %s listed", name)
		}
	}
}

func TestReplKeyActionsLowerCaseKebab(t *testing.T) {
	for _, name := range keyActionNames(t) {
		tt.Equal(t, "/^[a-z][a-z0-9-]*$/", name)
	}
}

func TestReplKeyActionsFreshList(t *testing.T) {
	// Destructively modifying the result must not change later results.
	withKeyBindings(t, "")
	tt.Equal(t, "t", keyEval(t, `(let ((a (repl-key-actions)))
                                    (setf (car a) 'zzz)
                                    (not (eq 'zzz (car (repl-key-actions)))))`))
}

func TestReplKeyActionsArgCount(t *testing.T) {
	(&sliptest.Function{Source: `(repl-key-actions 1)`, Panics: true}).Test(t)
}
