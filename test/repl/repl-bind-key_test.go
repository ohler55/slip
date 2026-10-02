// Copyright (c) 2026, Peter Ohler, All rights reserved.

package repl

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestReplBindKeyReturnsAction(t *testing.T) {
	withKeyBindings(t, "")
	(&sliptest.Function{
		Source: `(repl-bind-key "C-l" 'line-end)`,
		Expect: "line-end",
	}).Test(t)
}

func TestReplBindKeyNilReturnsNil(t *testing.T) {
	withKeyBindings(t, "")
	(&sliptest.Function{
		Source: `(repl-bind-key "C-l" nil)`,
		Expect: "nil",
	}).Test(t)
}

func TestReplBindKeyStoresEntry(t *testing.T) {
	withKeyBindings(t, `(repl-bind-key "C-l" 'line-end)`)
	tt.Equal(t, `(("C-l" . line-end))`, keyEval(t, `*repl-key-bindings*`))
}

func TestReplBindKeyNilStoresDisabledEntry(t *testing.T) {
	withKeyBindings(t, `(repl-bind-key "C-a" nil)`)
	tt.Equal(t, `(("C-a"))`, keyEval(t, `*repl-key-bindings*`))
	tt.Equal(t, "nil", keyEval(t, `(repl-key-binding "C-a")`))
}

func TestReplBindKeyAppendsInOrder(t *testing.T) {
	withKeyBindings(t, `(progn
                          (repl-bind-key "C-l" 'line-end)
                          (repl-bind-key "M-x" 'line-begin)
                          (repl-bind-key "C-g" nil))`)
	tt.Equal(t, `(("C-l" . line-end) ("M-x" . line-begin) ("C-g"))`, keyEval(t, `*repl-key-bindings*`))
}

func TestReplBindKeyReplacesInPlace(t *testing.T) {
	// Guess: replacing keeps the entry's position ("replaces an existing
	// entry ... else appends").
	withKeyBindings(t, `(progn
                          (repl-bind-key "C-l" 'line-end)
                          (repl-bind-key "M-x" 'line-begin)
                          (repl-bind-key "C-l" 'forward-word))`)
	tt.Equal(t, `(("C-l" . forward-word) ("M-x" . line-begin))`, keyEval(t, `*repl-key-bindings*`))
}

func TestReplBindKeyReplaceWithNil(t *testing.T) {
	withKeyBindings(t, `(progn (repl-bind-key "C-l" 'line-end) (repl-bind-key "C-l" nil))`)
	tt.Equal(t, `(("C-l"))`, keyEval(t, `*repl-key-bindings*`))
}

func TestReplBindKeyEveryAction(t *testing.T) {
	// Every name reported by repl-key-actions is accepted.
	withKeyBindings(t, "")
	tt.Equal(t, "t", keyEval(t, `(let ((ok t))
                                    (dolist (a (repl-key-actions) ok)
                                      (unless (eq a (repl-bind-key "C-l" a))
                                        (setq ok nil))
                                      (unless (eq a (repl-key-binding "C-l"))
                                        (setq ok nil))))`))
}

func TestReplBindKeyUnknownAction(t *testing.T) {
	withKeyBindings(t, `(repl-bind-key "C-l" 'line-end)`)
	p := keyError(t, `(repl-bind-key "C-g" 'no-such-action)`)
	tt.Equal(t, "/unknown action/", p.Error())
	tt.Equal(t, `(("C-l" . line-end))`, keyEval(t, `*repl-key-bindings*`))
}

func TestReplBindKeyUnknownActionOnExistingKey(t *testing.T) {
	withKeyBindings(t, `(repl-bind-key "C-l" 'line-end)`)
	p := keyError(t, `(repl-bind-key "C-l" 'no-such-action)`)
	tt.Equal(t, "/unknown action/", p.Error())
	tt.Equal(t, `(("C-l" . line-end))`, keyEval(t, `*repl-key-bindings*`))
}

func TestReplBindKeyInternalFuncsNotActions(t *testing.T) {
	// Internal table functions are not bindable actions.
	withKeyBindings(t, "")
	for _, name := range []string{"bad", "add-byte", "addByte", "top-uni", "add-uni", "esc", "esc5b"} {
		p := keyError(t, `(repl-bind-key "C-l" '`+name+`)`)
		tt.Equal(t, "/unknown action/", p.Error(), name)
	}
}

func TestReplBindKeyBadKeyType(t *testing.T) {
	withKeyBindings(t, "")
	(&sliptest.Function{
		Source:    `(repl-bind-key 7 'line-end)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(repl-bind-key 'c-l 'line-end)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	tt.Equal(t, "nil", keyEval(t, `*repl-key-bindings*`))
}

func TestReplBindKeyBadActionType(t *testing.T) {
	withKeyBindings(t, "")
	(&sliptest.Function{
		Source:    `(repl-bind-key "C-l" 7)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	// Guess: the action must be a symbol, a string naming it is a type error.
	(&sliptest.Function{
		Source:    `(repl-bind-key "C-l" "line-end")`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	tt.Equal(t, "nil", keyEval(t, `*repl-key-bindings*`))
}

func TestReplBindKeyArgCount(t *testing.T) {
	(&sliptest.Function{Source: `(repl-bind-key "C-l")`, Panics: true}).Test(t)
	(&sliptest.Function{Source: `(repl-bind-key "C-l" 'line-end 'x)`, Panics: true}).Test(t)
}
