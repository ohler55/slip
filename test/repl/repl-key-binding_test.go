// Copyright (c) 2026, Peter Ohler, All rights reserved.

package repl

import (
	"fmt"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

// Default bindings reported through the reverse lookup. Action names follow
// the list in the spec.
func TestReplKeyBindingDefaults(t *testing.T) {
	withKeyBindings(t, "")
	for _, x := range []struct {
		key    string
		action string
	}{
		{key: "C-a", action: "line-begin"},
		{key: "C-b", action: "back-char"},
		{key: "C-c", action: "exit"},
		{key: "C-d", action: "delete-char"},
		{key: "C-e", action: "line-end"},
		{key: "C-f", action: "forward-char"},
		{key: "C-h", action: "help"},
		{key: "TAB", action: "tab"},
		{key: "C-j", action: "newline"},
		{key: "C-k", action: "kill-line"},
		{key: "RET", action: "enter"},
		{key: "C-n", action: "next-line"},
		{key: "C-o", action: "newline-after"},
		{key: "C-p", action: "previous-line"},
		{key: "C-r", action: "search-history-back"},
		{key: "C-s", action: "search-history-forward"},
		{key: "C-t", action: "swap-chars"},
		{key: "C-u", action: "clear-form"},
		{key: "C-v", action: "history-forward"},
		{key: "C-w", action: "cut"},
		{key: "C-y", action: "paste"},
		{key: "C-_", action: "describe"},
		{key: "C-/", action: "describe"},
		{key: "DEL", action: "delete-back-char"},
		{key: "M-C-e", action: "edit-form"},
		{key: "M-,", action: "search-stash-back"},
		{key: "M-.", action: "search-stash-forward"},
		{key: "M-/", action: "describe"},
		{key: "M-?", action: "describe"},
		{key: "M-S", action: "nth-stash"},
		{key: "M-U", action: "enter-unicode"},
		{key: `M-\\`, action: "collapse-space"},
		{key: "M-b", action: "back-word"},
		{key: "M-d", action: "delete-word"},
		{key: "M-e", action: "eval-form"},
		{key: "M-f", action: "forward-word"},
		{key: "M-h", action: "nth-history"},
		{key: "M-n", action: "stash-forward"},
		{key: "M-p", action: "stash-back"},
		{key: "M-r", action: "reset-term"},
		{key: "M-s", action: "stash-add"},
		{key: "M-u", action: "enter-unicode"},
		{key: "M-v", action: "history-back"},
		{key: "M-w", action: "copy"},
		{key: "M-DEL", action: "delete-back-word"},
		{key: "M-[A", action: "previous-line"},
		{key: "M-[B", action: "next-line"},
		{key: "M-[C", action: "forward-char"},
		{key: "M-[D", action: "back-char"},
		{key: "M-[Z", action: "shift-tab"},
	} {
		t.Run(x.key, func(t *testing.T) {
			tt.Equal(t, x.action, keyEval(t, fmt.Sprintf(`(repl-key-binding "%s")`, x.key)))
		})
	}
}

func TestReplKeyBindingMatchParens(t *testing.T) {
	// Which of M-C-b / M-C-f is called match-open vs match-close is the
	// implementer's call; they must be the two distinct match actions.
	withKeyBindings(t, "")
	got := keyEval(t, `(list (repl-key-binding "M-C-b") (repl-key-binding "M-C-f"))`)
	tt.Equal(t, "/^\\((match-open match-close|match-close match-open)\\)$/", got)
}

func TestReplKeyBindingUnboundDefaults(t *testing.T) {
	withKeyBindings(t, "")
	for _, key := range []string{
		"C-l",     // bad in the root table
		"C-g",     // bad
		"C-@",     // bad
		"x",       // self insert is internal, not an action
		"M-x",     // bad under ESC
		"M-M-DEL", // bad
		"M-[1;5E", // unbound modified arrow
		"M-[1;2C", // unbound shift modifier
	} {
		t.Run(key, func(t *testing.T) {
			tt.Equal(t, "nil", keyEval(t, fmt.Sprintf(`(repl-key-binding "%s")`, key)))
		})
	}
}

func TestReplKeyBindingBasePrefixes(t *testing.T) {
	withKeyBindings(t, "")
	for _, key := range []string{"ESC", "C-[", "M-[", "M-[1", "M-[1;", "M-[1;5", "M-[3", "M-[3;5"} {
		t.Run(key, func(t *testing.T) {
			tt.Equal(t, "nil", keyEval(t, fmt.Sprintf(`(repl-key-binding "%s")`, key)))
		})
	}
}

func TestReplKeyBindingUserEntry(t *testing.T) {
	withKeyBindings(t, `(repl-bind-key "C-l" 'line-end)`)
	tt.Equal(t, "line-end", keyEval(t, `(repl-key-binding "C-l")`))
}

func TestReplKeyBindingUserOverridesDefault(t *testing.T) {
	withKeyBindings(t, `(repl-bind-key "M-[1;5C" 'line-end)`)
	tt.Equal(t, "line-end", keyEval(t, `(repl-key-binding "M-[1;5C")`))
	// Other keys with the same default action are unaffected.
	tt.Equal(t, "forward-word", keyEval(t, `(repl-key-binding "M-f")`))
}

func TestReplKeyBindingDisabledDefault(t *testing.T) {
	withKeyBindings(t, `(repl-bind-key "C-a" nil)`)
	tt.Equal(t, "nil", keyEval(t, `(repl-key-binding "C-a")`))
	tt.Equal(t, "line-end", keyEval(t, `(repl-key-binding "C-e")`))
}

func TestReplKeyBindingAfterClear(t *testing.T) {
	withKeyBindings(t, `(progn (repl-bind-key "C-a" 'line-end) (setq *repl-key-bindings* nil))`)
	tt.Equal(t, "line-begin", keyEval(t, `(repl-key-binding "C-a")`))
}

func TestReplKeyBindingBadType(t *testing.T) {
	(&sliptest.Function{
		Source:    `(repl-key-binding 7)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(repl-key-binding 'c-a)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestReplKeyBindingArgCount(t *testing.T) {
	(&sliptest.Function{Source: `(repl-key-binding)`, Panics: true}).Test(t)
	(&sliptest.Function{Source: `(repl-key-binding "C-a" "C-b")`, Panics: true}).Test(t)
}
