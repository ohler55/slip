// Copyright (c) 2026, Peter Ohler, All rights reserved.

package repl

import (
	"fmt"
	"regexp"
	"testing"

	"github.com/ohler55/ojg/tt"
)

// Terminal keys that are bindable but have no default (addendum B1).
// name is the raw spelling, shown the name used in messages (addendum D5).
var keyTerminalKeys = []struct {
	name  string
	seq   string
	shown string
}{
	{name: "M-[2~", seq: "\x1b[2~", shown: "<insert>"}, // Insert
	{name: "M-[5~", seq: "\x1b[5~", shown: "<prior>"},  // PgUp
	{name: "M-[6~", seq: "\x1b[6~", shown: "<next>"},   // PgDn
	{name: "M-[15~", seq: "\x1b[15~", shown: "<f5>"},   // F5
	{name: "M-[17~", seq: "\x1b[17~", shown: "<f6>"},   // F6
	{name: "M-[18~", seq: "\x1b[18~", shown: "<f7>"},   // F7
	{name: "M-[19~", seq: "\x1b[19~", shown: "<f8>"},   // F8
	{name: "M-[20~", seq: "\x1b[20~", shown: "<f9>"},   // F9
	{name: "M-[21~", seq: "\x1b[21~", shown: "<f10>"},  // F10
	{name: "M-[23~", seq: "\x1b[23~", shown: "<f11>"},  // F11
	{name: "M-[24~", seq: "\x1b[24~", shown: "<f12>"},  // F12
	{name: "M-OP", seq: "\x1bOP", shown: "<f1>"},       // F1
	{name: "M-OQ", seq: "\x1bOQ", shown: "<f2>"},       // F2
	{name: "M-OR", seq: "\x1bOR", shown: "<f3>"},       // F3
	{name: "M-OS", seq: "\x1bOS", shown: "<f4>"},       // F4
	{name: "M-OH", seq: "\x1bOH", shown: "<home>"},
	{name: "M-OF", seq: "\x1bOF", shown: "<end>"},
	{name: "M-OA", seq: "\x1bOA", shown: "<up>"},
	{name: "M-OB", seq: "\x1bOB", shown: "<down>"},
	{name: "M-OC", seq: "\x1bOC", shown: "<right>"},
	{name: "M-OD", seq: "\x1bOD", shown: "<left>"},
}

// Keys whose defaults were removed again (addendum C1): still bindable, but
// undefined until bound.
var keyC1Keys = []struct {
	name  string
	seq   string
	shown string
}{
	{name: "M-[1;5C", seq: "\x1b[1;5C", shown: "C-<right>"},  // Ctrl-Right
	{name: "M-[1;5D", seq: "\x1b[1;5D", shown: "C-<left>"},   // Ctrl-Left
	{name: "M-[1;3C", seq: "\x1b[1;3C", shown: "M-<right>"},  // Alt-Right
	{name: "M-[1;3D", seq: "\x1b[1;3D", shown: "M-<left>"},   // Alt-Left
	{name: "M-[1;5A", seq: "\x1b[1;5A", shown: "C-<up>"},     // Ctrl-Up
	{name: "M-[1;5B", seq: "\x1b[1;5B", shown: "C-<down>"},   // Ctrl-Down
	{name: "M-[1;3A", seq: "\x1b[1;3A", shown: "M-<up>"},     // Alt-Up
	{name: "M-[1;3B", seq: "\x1b[1;3B", shown: "M-<down>"},   // Alt-Down
	{name: "M-[H", seq: "\x1b[H", shown: "<home>"},           // Home
	{name: "M-[F", seq: "\x1b[F", shown: "<end>"},            // End
	{name: "M-[1~", seq: "\x1b[1~", shown: "<home>"},         // Home
	{name: "M-[4~", seq: "\x1b[4~", shown: "<end>"},          // End
	{name: "M-[3~", seq: "\x1b[3~", shown: "<delete>"},       // Delete
	{name: "M-[3;5~", seq: "\x1b[3;5~", shown: "C-<delete>"}, // Ctrl-Delete
}

// keyUnboundKeys is every bindable key without a default.
var keyUnboundKeys = append(append(keyC1Keys[:0:0], keyTerminalKeys...), keyC1Keys...)

func TestKeyTermNoDefault(t *testing.T) {
	withKeyBindings(t, "")
	for _, k := range keyUnboundKeys {
		tt.Equal(t, "nil", keyEval(t, fmt.Sprintf(`(repl-key-binding "%s")`, k.name)), k.name)
	}
}

func TestKeyTermUndefinedInEditor(t *testing.T) {
	for _, k := range keyUnboundKeys {
		t.Run(k.name, func(t *testing.T) {
			withKeyBindings(t, "")
			keyEdTest(t, []any{
				startSteps,
				provide(k.seq),
				until("<inverse>"),
				expect("  "),
				expect("/^key " + regexp.QuoteMeta(k.shown) + " is undefined. sequence: /"),
			})
		})
	}
}

func TestKeyTermBindable(t *testing.T) {
	for _, k := range keyUnboundKeys {
		t.Run(k.name, func(t *testing.T) {
			withKeyBindings(t, "")
			tt.Equal(t, "line-end", keyEval(t, fmt.Sprintf(`(repl-bind-key "%s" 'line-end)`, k.name)))
			tt.Equal(t, "line-end", keyEval(t, fmt.Sprintf(`(repl-key-binding "%s")`, k.name)))
		})
	}
}

func TestKeyTermBoundWorksInEditor(t *testing.T) {
	for _, k := range keyUnboundKeys {
		t.Run(k.name, func(t *testing.T) {
			withKeyBindings(t, fmt.Sprintf(`(repl-bind-key "%s" 'line-begin)`, k.name))
			keyEdTest(t, []any{
				startSteps,
				provide("abc"),
				until("c"),
				until("<set-cursor 2:6>"),
				provide(k.seq),
				untilWithout{target: "<set-cursor 2:3>", forbid: "/undefined/"},
				comment("back in the top mode"),
				provide("x"),
				untilWithout{target: "xabc", forbid: "/undefined/"},
			})
		})
	}
}

func TestKeyTermBoundWorksSplitReads(t *testing.T) {
	withKeyBindings(t, `(progn (repl-bind-key "M-[15~" 'line-begin) (repl-bind-key "M-OP" 'line-end))`)
	keyEdTest(t, []any{
		startSteps,
		provide("abc"),
		until("c"),
		until("<set-cursor 2:6>"),
		provide("\x1b["),
		provide("1"),
		provide("5~"),
		untilWithout{target: "<set-cursor 2:3>", forbid: "/undefined/"},
		provide("\x1b"),
		provide("O"),
		provide("P"),
		untilWithout{target: "<set-cursor 2:6>", forbid: "/undefined/"},
	})
}

func TestKeyTermPrefixesRefused(t *testing.T) {
	for _, key := range []string{"M-O", "M-[2", "M-[5", "M-[6", "M-[1", "M-[15", "M-[17", "M-[20", "M-[23", "M-[24"} {
		t.Run(key, func(t *testing.T) {
			withKeyBindings(t, "")
			p := keyError(t, fmt.Sprintf(`(repl-bind-key "%s" 'line-end)`, key))
			tt.Equal(t, "/^key .*prefix/", p.Error(), key)
			tt.Equal(t, "nil", keyEval(t, `*repl-key-bindings*`))
			tt.Equal(t, "nil", keyEval(t, fmt.Sprintf(`(repl-key-binding "%s")`, key)))
		})
	}
}

// Alt-O is now a prefix: Alt-O followed by another key reports the pair as
// undefined.
func TestKeyTermAltOThenKey(t *testing.T) {
	withKeyBindings(t, "")
	keyEdTest(t, []any{
		startSteps,
		provide("\x1bOx"),
		until("<inverse>"),
		expect("  "),
		expect("/^key M-Ox is undefined. sequence: /"),
	})
}

func TestKeyTermAltOThenKeySplitReads(t *testing.T) {
	withKeyBindings(t, "")
	keyEdTest(t, []any{
		startSteps,
		provide("\x1bO"),
		provide("x"),
		until("<inverse>"),
		expect("  "),
		expect("/^key M-Ox is undefined/"),
		comment("the editor is usable afterwards"),
		provide("y"),
		untilWithout{target: "y", forbid: "/undefined/"},
	})
}

func TestKeyTermAltOBindable(t *testing.T) {
	// A pair under the new M-O prefix can itself be bound.
	withKeyBindings(t, `(repl-bind-key "M-Ox" 'line-begin)`)
	keyEdSeq(t, "abc\x1bOx", "a", "b", "c", "<set-cursor 2:3>")
}

// Delete key (addendum B2, with the default removed by addendum C1): the
// delete-forward action is bound to M-[3~ by the user.

const keyBindDelete = `(repl-bind-key "M-[3~" 'delete-forward)`

func TestKeyTermDeleteBinding(t *testing.T) {
	withKeyBindings(t, "")
	tt.Equal(t, "nil", keyEval(t, `(repl-key-binding "M-[3~")`))
	tt.Equal(t, "delete-char", keyEval(t, `(repl-key-binding "C-d")`))
	tt.Equal(t, "t", keyEval(t, `(not (null (member 'delete-forward (repl-key-actions))))`))
	_ = keyEval(t, keyBindDelete)
	tt.Equal(t, "delete-forward", keyEval(t, `(repl-key-binding "M-[3~")`))
}

func TestKeyTermDeleteDeletesForward(t *testing.T) {
	withKeyBindings(t, keyBindDelete)
	keyEdTest(t, []any{
		startSteps,
		provide("abc"),
		until("c"),
		provide("\x01"),
		until("<set-cursor 2:3>"),
		provide("\x1b[3~"),
		untilWithout{target: "bc", forbid: "/undefined/"},
		until("<set-cursor 2:3>"),
	})
}

func TestKeyTermDeleteOnEmptyFormDoesNotExit(t *testing.T) {
	withKeyBindings(t, keyBindDelete)
	keyEdTest(t, []any{
		startSteps,
		provide("\x1b[3~"),
		provide("x"),
		untilWithout{target: "x", forbid: "/Bye|undefined/"},
	})
}

func TestKeyTermDeleteAtLineEnd(t *testing.T) {
	withKeyBindings(t, keyBindDelete)
	keyEdTest(t, []any{
		startSteps,
		provide("abc"),
		until("c"),
		provide("\x1b[3~"),
		provide("x"),
		untilWithout{target: "x", forbid: "/Bye|undefined/"},
	})
}

func TestKeyTermUserDeleteForwardDoesNotExit(t *testing.T) {
	withKeyBindings(t, `(repl-bind-key "C-l" 'delete-forward)`)
	keyEdTest(t, []any{
		startSteps,
		provide("\x0c"),
		provide("x"),
		untilWithout{target: "x", forbid: "/Bye|undefined/"},
	})
}

func TestKeyTermCtrlDOnEmptyFormExits(t *testing.T) {
	withKeyBindings(t, "")
	keyEdTest(t, []any{
		startSteps,
		provide("\x04"),
		until("/Bye/"),
	})
}

func TestKeyTermCtrlDStillDeletes(t *testing.T) {
	withKeyBindings(t, "")
	keyEdSeq(t, "abc\x01\x04", "<set-cursor 2:3>", "bc", "<set-cursor 2:3>")
}
