// Copyright (c) 2026, Peter Ohler, All rights reserved.

package repl

import (
	"fmt"
	"testing"

	"github.com/ohler55/ojg/tt"
)

// Prefix clash rules (decision 6). A refused binding raises an error whose
// message starts with "key <name> " and contains "prefix", and leaves the
// bindings unchanged.

func requirePrefixError(t *testing.T, key string) {
	t.Helper()
	p := keyError(t, fmt.Sprintf(`(repl-bind-key "%s" 'line-end)`, key))
	tt.Equal(t, "/^key .*prefix/", p.Error(), key)
}

// Rule 1: the key is itself a base prefix.
func TestKeyPrefixRule1BasePrefix(t *testing.T) {
	for _, key := range []string{
		"ESC", "C-[", "M-[", "M-[1", "M-[1;", "M-[1;5", "M-[1;3",
		"M-[3", "M-[3;", "M-[3;5", "M-[4",
	} {
		t.Run(key, func(t *testing.T) {
			withKeyBindings(t, "")
			requirePrefixError(t, key)
			tt.Equal(t, "nil", keyEval(t, `*repl-key-bindings*`))
		})
	}
}

func TestKeyPrefixRule1NamesKey(t *testing.T) {
	withKeyBindings(t, "")
	p := keyError(t, `(repl-bind-key "M-[1;5" 'line-end)`)
	tt.Equal(t, "/^key M-\\[1;5 /", p.Error())
}

// Rule 2: a proper prefix is a key in its own right (not a base prefix). The
// message may speak of a prefix or of chords (addendum A item 9).
func TestKeyPrefixRule2ShadowsKey(t *testing.T) {
	for _, key := range []string{
		"C-ax",     // C-a is line-begin
		"xy",       // x is self insert
		"M-fx",     // M-f is forward-word
		"M-[Ax",    // M-[A is the up arrow
		"M-[1;5Cx", // M-[1;5C is a key (no default, but not a prefix)
		"C-lx",     // C-l is undefined by default but still a key
		"M-[Zx",    // M-[Z is shift-tab
	} {
		t.Run(key, func(t *testing.T) {
			withKeyBindings(t, "")
			p := keyError(t, fmt.Sprintf(`(repl-bind-key "%s" 'line-end)`, key))
			tt.Equal(t, "/^key .*(prefix|chord)/", p.Error(), key)
			tt.Equal(t, "nil", keyEval(t, `*repl-key-bindings*`))
		})
	}
}

// Addendum A item 9: multi-key chords such as M-M-x are refused with a
// message saying chords are not supported.
func TestKeyPrefixMetaMetaChordRefused(t *testing.T) {
	withKeyBindings(t, "")
	p := keyError(t, `(repl-bind-key "M-M-x" 'line-end)`)
	tt.Equal(t, "/^key M-M-x .*chord/", p.Error())
	tt.Equal(t, "nil", keyEval(t, `*repl-key-bindings*`))
}

func TestKeyPrefixMetaMetaChordRefusedInVariable(t *testing.T) {
	withKeyBindings(t, `(repl-bind-key "C-l" 'line-end)`)
	p := keyError(t, `(setq *repl-key-bindings* '(("M-M-x" . line-end)))`)
	tt.Equal(t, "/chord/", p.Error())
	tt.Equal(t, `(("C-l" . line-end))`, keyEval(t, `*repl-key-bindings*`))
}

// Rule 3: the new key is a proper prefix of a user binding or the other way
// around. With rules 1 and 2 in place any such case is also caught by one of
// them (a proper prefix of a bound key must be a base prefix, so rule 1 fires
// for the shorter key and rule 2 for the longer one); these tests pin the
// observable behavior for both orders.
func TestKeyPrefixRule3NewIsPrefixOfUser(t *testing.T) {
	withKeyBindings(t, `(repl-bind-key "M-[1;5C" 'line-end)`)
	requirePrefixError(t, "M-[1;5")
	tt.Equal(t, `(("M-[1;5C" . line-end))`, keyEval(t, `*repl-key-bindings*`))
}

func TestKeyPrefixRule3UserIsPrefixOfNew(t *testing.T) {
	withKeyBindings(t, `(repl-bind-key "M-x" 'line-end)`)
	requirePrefixError(t, "M-xy")
	tt.Equal(t, `(("M-x" . line-end))`, keyEval(t, `*repl-key-bindings*`))
}

func TestKeyPrefixRule3DisabledUserIsPrefixOfNew(t *testing.T) {
	withKeyBindings(t, `(repl-bind-key "C-l" nil)`)
	requirePrefixError(t, "C-lx")
	tt.Equal(t, `(("C-l"))`, keyEval(t, `*repl-key-bindings*`))
}

func TestKeyPrefixAllowed(t *testing.T) {
	for _, key := range []string{
		"M-[1;5C", // no default, under base prefixes
		"M-f",     // under ESC
		"C-l",     // undefined single key
		"C-a",     // default single key
		"M-[3;5~", // no default
		"M-[1;5E", // unbound, all proper prefixes are base prefixes
		"M-[Q",    // unbound under ESC [
		"M-x",
		"M-C-b",
		"M-ESC", // ESC ESC is a key, its only proper prefix is ESC
	} {
		t.Run(key, func(t *testing.T) {
			withKeyBindings(t, "")
			tt.Equal(t, "line-end", keyEval(t, fmt.Sprintf(`(repl-bind-key "%s" 'line-end)`, key)))
		})
	}
}

func TestKeyPrefixSiblingsAllowed(t *testing.T) {
	// Keys sharing only base prefixes do not clash.
	withKeyBindings(t, `(progn
                          (repl-bind-key "M-[1;5C" 'line-end)
                          (repl-bind-key "M-[1;5D" 'line-begin)
                          (repl-bind-key "M-x" 'forward-word)
                          (repl-bind-key "M-y" 'back-word))`)
	tt.Equal(t, "4", keyEval(t, `(length *repl-key-bindings*)`))
}

func TestKeyPrefixRebindSameKey(t *testing.T) {
	// Rebinding a key that already has a user binding replaces it; the key is
	// not a proper prefix of itself.
	withKeyBindings(t, `(progn (repl-bind-key "M-x" 'line-end) (repl-bind-key "M-x" 'line-begin))`)
	tt.Equal(t, `(("M-x" . line-begin))`, keyEval(t, `*repl-key-bindings*`))
}

func TestKeyPrefixThroughVariable(t *testing.T) {
	withKeyBindings(t, `(repl-bind-key "C-l" 'line-end)`)
	p := keyError(t, `(setq *repl-key-bindings* '(("M-x" . line-end) ("M-[" . line-begin)))`)
	tt.Equal(t, "/^key .*prefix/", p.Error())
	tt.Equal(t, `(("C-l" . line-end))`, keyEval(t, `*repl-key-bindings*`))
}

func TestKeyPrefixWithinVariableList(t *testing.T) {
	// Rule 2 against the tables within one list.
	withKeyBindings(t, "")
	p := keyError(t, `(setq *repl-key-bindings* '(("M-x" . line-end) ("C-ax" . line-begin)))`)
	tt.Equal(t, "/^key .*prefix/", p.Error())
	tt.Equal(t, "nil", keyEval(t, `*repl-key-bindings*`))
}

// A proper prefix that is not a key prefix at all (an undefined key such as
// ESC [ 7 or ESC [ 1 6) is refused as "not a key prefix"; a proper prefix that is
// itself a key is refused as a chord.
func TestKeyPrefixNotAKeyPrefix(t *testing.T) {
	// M-[5~ and M-OP became bindable (addendum B1); M-[7 and M-[9 are still
	// unknown to the editor.
	for _, key := range []string{"M-[7~", "M-[9~", "M-[16~"} {
		t.Run(key, func(t *testing.T) {
			withKeyBindings(t, "")
			p := keyError(t, fmt.Sprintf(`(repl-bind-key "%s" 'line-end)`, key))
			tt.Equal(t, "/not a key prefix/", p.Error(), key)
			tt.Equal(t, "nil", keyEval(t, `*repl-key-bindings*`))
		})
	}
}

func TestKeyPrefixChordMessages(t *testing.T) {
	for _, key := range []string{"M-M-x", "C-ax"} {
		t.Run(key, func(t *testing.T) {
			withKeyBindings(t, "")
			p := keyError(t, fmt.Sprintf(`(repl-bind-key "%s" 'line-end)`, key))
			tt.Equal(t, "/chord/", p.Error(), key)
		})
	}
}
