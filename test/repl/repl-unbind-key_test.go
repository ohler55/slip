// Copyright (c) 2026, Peter Ohler, All rights reserved.

package repl

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestReplUnbindKeyRemovesEntry(t *testing.T) {
	withKeyBindings(t, `(repl-bind-key "C-l" 'line-end)`)
	(&sliptest.Function{
		Source: `(repl-unbind-key "C-l")`,
		Expect: "t",
	}).Test(t)
	tt.Equal(t, "nil", keyEval(t, `*repl-key-bindings*`))
}

func TestReplUnbindKeyMissing(t *testing.T) {
	withKeyBindings(t, `(repl-bind-key "C-l" 'line-end)`)
	(&sliptest.Function{
		Source: `(repl-unbind-key "C-g")`,
		Expect: "nil",
	}).Test(t)
	tt.Equal(t, `(("C-l" . line-end))`, keyEval(t, `*repl-key-bindings*`))
}

func TestReplUnbindKeyEmpty(t *testing.T) {
	withKeyBindings(t, "")
	(&sliptest.Function{
		Source: `(repl-unbind-key "C-a")`,
		Expect: "nil",
	}).Test(t)
}

func TestReplUnbindKeyTwice(t *testing.T) {
	withKeyBindings(t, `(repl-bind-key "C-l" 'line-end)`)
	tt.Equal(t, "(t nil)", keyEval(t, `(list (repl-unbind-key "C-l") (repl-unbind-key "C-l"))`))
}

func TestReplUnbindKeyRestoresDefault(t *testing.T) {
	withKeyBindings(t, `(repl-bind-key "C-a" 'line-end)`)
	tt.Equal(t, "line-end", keyEval(t, `(repl-key-binding "C-a")`))
	_ = keyEval(t, `(repl-unbind-key "C-a")`)
	tt.Equal(t, "line-begin", keyEval(t, `(repl-key-binding "C-a")`))
}

func TestReplUnbindKeyRestoresDisabledDefault(t *testing.T) {
	withKeyBindings(t, `(repl-bind-key "M-f" nil)`)
	tt.Equal(t, "nil", keyEval(t, `(repl-key-binding "M-f")`))
	tt.Equal(t, "t", keyEval(t, `(repl-unbind-key "M-f")`))
	tt.Equal(t, "forward-word", keyEval(t, `(repl-key-binding "M-f")`))
}

func TestReplUnbindKeyKeepsOthersInOrder(t *testing.T) {
	withKeyBindings(t, `(progn
                          (repl-bind-key "C-l" 'line-end)
                          (repl-bind-key "M-x" 'line-begin)
                          (repl-bind-key "C-g" nil))`)
	_ = keyEval(t, `(repl-unbind-key "M-x")`)
	tt.Equal(t, `(("C-l" . line-end) ("C-g"))`, keyEval(t, `*repl-key-bindings*`))
}

func TestReplUnbindKeyAlternateSpelling(t *testing.T) {
	// C-/ and C-_ name the same byte.
	withKeyBindings(t, `(repl-bind-key "C-_" 'line-end)`)
	tt.Equal(t, "t", keyEval(t, `(repl-unbind-key "C-/")`))
	tt.Equal(t, "nil", keyEval(t, `*repl-key-bindings*`))
}

func TestReplUnbindKeyThenRebind(t *testing.T) {
	// Removing then adding again appends at the end.
	withKeyBindings(t, `(progn
                          (repl-bind-key "C-l" 'line-end)
                          (repl-bind-key "M-x" 'line-begin)
                          (repl-unbind-key "C-l")
                          (repl-bind-key "C-l" 'forward-word))`)
	tt.Equal(t, `(("M-x" . line-begin) ("C-l" . forward-word))`, keyEval(t, `*repl-key-bindings*`))
}

func TestReplUnbindKeyAllowsFormerPrefixUse(t *testing.T) {
	// Once M-x is unbound the layer no longer holds anything under it.
	withKeyBindings(t, `(progn (repl-bind-key "M-x" 'line-end) (repl-unbind-key "M-x"))`)
	tt.Equal(t, "line-begin", keyEval(t, `(repl-bind-key "M-y" 'line-begin)`))
}

func TestReplUnbindKeyBadType(t *testing.T) {
	withKeyBindings(t, "")
	(&sliptest.Function{
		Source:    `(repl-unbind-key 7)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(repl-unbind-key nil)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestReplUnbindKeyArgCount(t *testing.T) {
	(&sliptest.Function{Source: `(repl-unbind-key)`, Panics: true}).Test(t)
	(&sliptest.Function{Source: `(repl-unbind-key "C-l" "C-g")`, Panics: true}).Test(t)
}
