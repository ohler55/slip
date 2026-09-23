// Copyright (c) 2026, Peter Ohler, All rights reserved.

package repl

import (
	"fmt"
	"strings"
	"testing"

	"github.com/ohler55/ojg/tt"
)

// Key name grammar, observed through repl-bind-key and the
// *repl-key-bindings* value that holds the canonical (formatKey) name.

// canonicalKeyName binds key and returns the name stored in
// *repl-key-bindings*.
func canonicalKeyName(t *testing.T, key string) string {
	t.Helper()
	withKeyBindings(t, fmt.Sprintf(`(repl-bind-key %q 'line-end)`, key))
	return keyEval(t, `(car (car *repl-key-bindings*))`)
}

func TestKeyNameCanonical(t *testing.T) {
	for _, x := range []struct {
		key  string
		want string
	}{
		{key: "C-l", want: `"C-l"`},
		{key: "C-a", want: `"C-a"`},
		{key: "C-z", want: `"C-z"`},
		{key: "M-x", want: `"M-x"`},
		{key: "M-C-b", want: `"M-C-b"`},
		{key: "M-[1;5C", want: `"M-[1;5C"`},
		{key: "M-[3;5~", want: `"M-[3;5~"`},
		{key: "M-DEL", want: `"M-DEL"`},
		{key: "DEL", want: `"DEL"`},
		{key: "TAB", want: `"TAB"`},
		{key: "RET", want: `"RET"`},
		{key: "SPC", want: `"SPC"`},
		{key: "x", want: `"x"`},
		{key: "~", want: `"~"`},
		{key: "!", want: `"!"`},
		// Named keys win over the C- form for the same byte (guess: the spec
		// lists 0x09 as TAB and 0x0d as RET in formatKey).
		{key: "C-i", want: `"TAB"`},
		{key: "C-m", want: `"RET"`},
		{key: "M-C-i", want: `"M-TAB"`},
		// Synonyms (addendum A item 9).
		{key: "C-M-b", want: `"M-C-b"`},
		{key: "C-A", want: `"C-a"`},
		{key: "C-Z", want: `"C-z"`},
		{key: "C-C", want: `"C-c"`},
		{key: "C-M-B", want: `"M-C-b"`},
	} {
		t.Run(x.key, func(t *testing.T) {
			tt.Equal(t, x.want, canonicalKeyName(t, x.key))
		})
	}
}

// For control bytes whose formatted name the spec leaves open (0x00, 0x1c to
// 0x1f and a trailing ESC) only the round trip is checked: binding, reading
// the variable back, clearing, and setting that value again reproduces the
// binding.
func TestKeyNameRoundTrip(t *testing.T) {
	for _, key := range []string{
		`C-@`, `C-\\`, `C-]`, `C-^`, `C-_`, `C-/`, `M-C-b`, `M-ESC`,
		`M-[1;5C`, `M-[3;5~`, `DEL`, `TAB`, `RET`, `SPC`, `x`, `M-C-@`,
	} {
		t.Run(key, func(t *testing.T) {
			withKeyBindings(t, "")
			_ = keyEval(t, fmt.Sprintf(`(repl-bind-key "%s" 'line-begin)`, key))
			got := keyEval(t, `(let ((b *repl-key-bindings*))
                                 (setq *repl-key-bindings* nil)
                                 (setq *repl-key-bindings* b)
                                 b)`)
			tt.Equal(t, 1, strings.Count(got, "line-begin"), got)
			tt.Equal(t, "line-begin", keyEval(t, fmt.Sprintf(`(repl-key-binding "%s")`, key)))
			stored := keyEval(t, `(repl-key-binding (car (car *repl-key-bindings*)))`)
			tt.Equal(t, "line-begin", stored)
		})
	}
}

func TestKeyNameControlAliases(t *testing.T) {
	// C-_ and C-/ are both 0x1f so the second bind replaces the first.
	withKeyBindings(t, `(progn (repl-bind-key "C-_" 'line-end) (repl-bind-key "C-/" 'line-begin))`)
	tt.Equal(t, "1", keyEval(t, `(length *repl-key-bindings*)`))
	tt.Equal(t, "line-begin", keyEval(t, `(repl-key-binding "C-_")`))
}

func TestKeyNameTabAlias(t *testing.T) {
	withKeyBindings(t, `(progn (repl-bind-key "C-i" 'line-end) (repl-bind-key "TAB" 'line-begin))`)
	tt.Equal(t, "1", keyEval(t, `(length *repl-key-bindings*)`))
	tt.Equal(t, "line-begin", keyEval(t, `(repl-key-binding "C-i")`))
}

func TestKeyNameEscAlias(t *testing.T) {
	// M-x, ESC x and C-[ x are the same bytes.
	withKeyBindings(t, `(progn (repl-bind-key "M-x" 'line-end) (repl-bind-key "ESCx" 'line-begin))`)
	tt.Equal(t, "1", keyEval(t, `(length *repl-key-bindings*)`))
	tt.Equal(t, "line-begin", keyEval(t, `(repl-key-binding "C-[x")`))
}

func TestKeyNameInvalid(t *testing.T) {
	for _, x := range []struct {
		name string
		key  string
	}{
		{name: "empty", key: `""`},
		{name: "dangling C-", key: `"C-"`},
		{name: "dangling M-", key: `"M-"`},
		{name: "dangling M-C-", key: `"M-C-"`},
		{name: "dangling C-M-", key: `"C-M-"`},
		{name: "dangling M-M-", key: `"M-M-"`},
		{name: "dangling C- after key", key: `"xC-"`},
		{name: "C- digit", key: `"C-1"`},
		{name: "C- space", key: `"C- "`},
		{name: "C- tilde", key: `"C-~"`},
		// C- may only be followed by M- (C-M-x means M-C-x), so C-C-a is
		// refused rather than read as C-c - a.
		{name: "C-C-", key: `"C-C-a"`},
		{name: "C-M-C-", key: `"C-M-C-a"`},
		{name: "C-C-M-", key: `"C-C-M-a"`},
		{name: "C-M-M-", key: `"C-M-M-a"`},
		{name: "non-ASCII", key: `"é"`},
		{name: "M- non-ASCII", key: `"M-é"`},
		{name: "C- non-ASCII", key: `"C-é"`},
		// The reader rejects raw control characters in string literals so
		// these are built with code-char.
		{name: "literal control", key: `(string (code-char 1))`},
		{name: "literal DEL", key: `(string (code-char 127))`},
		{name: "literal ESC", key: `(string (code-char 27))`},
		{name: "literal tab", key: `(string (code-char 9))`},
		{name: "literal NUL", key: `(string (code-char 0))`},
		{name: "M- then literal control", key: `(concatenate 'string "M-" (string (code-char 2)))`},
		{name: "literal high byte", key: `(string (code-char 255))`},
	} {
		t.Run(x.name, func(t *testing.T) {
			withKeyBindings(t, "")
			for _, form := range []string{
				`(repl-bind-key %s 'line-end)`,
				`(repl-unbind-key %s)`,
				`(repl-key-binding %s)`,
				`(setq *repl-key-bindings* (list (cons %s 'line-end)))`,
			} {
				src := fmt.Sprintf(form, x.key)
				p := keyError(t, src)
				tt.Equal(t, "/^invalid key/", p.Error(), src)
			}
			tt.Equal(t, "nil", keyEval(t, `*repl-key-bindings*`))
		})
	}
}

func TestKeyNameUpperCaseControlSynonym(t *testing.T) {
	withKeyBindings(t, `(progn (repl-bind-key "C-l" 'line-end) (repl-bind-key "C-L" 'line-begin))`)
	tt.Equal(t, `(("C-l" . line-begin))`, keyEval(t, `*repl-key-bindings*`))
	tt.Equal(t, "line-begin", keyEval(t, `(repl-key-binding "C-A")`))
}

func TestKeyNameCtrlMetaSynonym(t *testing.T) {
	withKeyBindings(t, `(progn (repl-bind-key "M-C-b" 'line-end) (repl-bind-key "C-M-b" 'line-begin))`)
	tt.Equal(t, `(("M-C-b" . line-begin))`, keyEval(t, `*repl-key-bindings*`))
	tt.Equal(t, "line-begin", keyEval(t, `(repl-key-binding "C-M-b")`))
}

func TestKeyNameMetaMetaParses(t *testing.T) {
	// M-M-x parses (it is needed for messages) even though it can't be bound.
	withKeyBindings(t, "")
	tt.Equal(t, "nil", keyEval(t, `(repl-key-binding "M-M-x")`))
	tt.Equal(t, "nil", keyEval(t, `(repl-unbind-key "M-M-x")`))
}
