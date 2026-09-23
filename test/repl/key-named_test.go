// Copyright (c) 2026, Peter Ohler, All rights reserved.

package repl

import (
	"fmt"
	"regexp"
	"strings"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

// Named terminal keys (addendum D).

// keyNamed lists each D1 name, its aliases and all of its encodings.
var keyNamed = []struct {
	name    string
	aliases []string
	seqs    []string
}{
	{name: "<up>", seqs: []string{"\x1b[A", "\x1bOA"}},
	{name: "<down>", seqs: []string{"\x1b[B", "\x1bOB"}},
	{name: "<right>", seqs: []string{"\x1b[C", "\x1bOC"}},
	{name: "<left>", seqs: []string{"\x1b[D", "\x1bOD"}},
	{name: "<home>", seqs: []string{"\x1b[H", "\x1b[1~", "\x1bOH"}},
	{name: "<end>", seqs: []string{"\x1b[F", "\x1b[4~", "\x1bOF"}},
	{name: "<insert>", seqs: []string{"\x1b[2~"}},
	{name: "<delete>", seqs: []string{"\x1b[3~"}},
	{name: "<prior>", aliases: []string{"<pageup>"}, seqs: []string{"\x1b[5~"}},
	{name: "<next>", aliases: []string{"<pagedown>"}, seqs: []string{"\x1b[6~"}},
	{name: "<f1>", seqs: []string{"\x1bOP"}},
	{name: "<f2>", seqs: []string{"\x1bOQ"}},
	{name: "<f3>", seqs: []string{"\x1bOR"}},
	{name: "<f4>", seqs: []string{"\x1bOS"}},
	{name: "<f5>", seqs: []string{"\x1b[15~"}},
	{name: "<f6>", seqs: []string{"\x1b[17~"}},
	{name: "<f7>", seqs: []string{"\x1b[18~"}},
	{name: "<f8>", seqs: []string{"\x1b[19~"}},
	{name: "<f9>", seqs: []string{"\x1b[20~"}},
	{name: "<f10>", seqs: []string{"\x1b[21~"}},
	{name: "<f11>", seqs: []string{"\x1b[23~"}},
	{name: "<f12>", seqs: []string{"\x1b[24~"}},
	{name: "S-<tab>", aliases: []string{"<backtab>"}, seqs: []string{"\x1b[Z"}},
}

// keyPressMovesToStart types abc, presses seq and expects the cursor at the
// line start (the key is bound to line-begin), then that typing works.
func keyPressMovesToStart(seq string) []any {
	return []any{
		startSteps,
		provide("abc"),
		until("c"),
		until("<set-cursor 2:6>"),
		provide(seq),
		untilWithout{target: "<set-cursor 2:3>", forbid: "/undefined/"},
		provide("x"),
		untilWithout{target: "xabc", forbid: "/undefined/"},
	}
}

// D1: a name (or alias) binds every one of its encodings.
func TestKeyNamedBindsAllEncodings(t *testing.T) {
	for _, k := range keyNamed {
		for _, spelling := range append([]string{k.name}, k.aliases...) {
			for _, seq := range k.seqs {
				t.Run(fmt.Sprintf("%s/%q", spelling, seq), func(t *testing.T) {
					withKeyBindings(t, fmt.Sprintf(`(repl-bind-key "%s" 'line-begin)`, spelling))
					keyEdTest(t, keyPressMovesToStart(seq))
				})
			}
		}
	}
}

func TestKeyNamedRawEncodingsReportAction(t *testing.T) {
	// After binding a name, every raw spelling of its encodings reports the
	// action.
	for _, k := range keyNamed {
		t.Run(k.name, func(t *testing.T) {
			withKeyBindings(t, fmt.Sprintf(`(repl-bind-key "%s" 'line-end)`, k.name))
			for _, seq := range k.seqs {
				raw := "M-" + seq[1:]
				tt.Equal(t, "line-end", keyEval(t, fmt.Sprintf(`(repl-key-binding "%s")`, raw)), raw)
			}
			tt.Equal(t, "line-end", keyEval(t, fmt.Sprintf(`(repl-key-binding "%s")`, k.name)))
		})
	}
}

func TestKeyNamedAliasesStoredCanonically(t *testing.T) {
	for _, k := range keyNamed {
		for _, alias := range k.aliases {
			t.Run(alias, func(t *testing.T) {
				withKeyBindings(t, fmt.Sprintf(`(repl-bind-key "%s" 'line-end)`, alias))
				tt.Equal(t, fmt.Sprintf(`(("%s" . line-end))`, k.name), keyEval(t, `*repl-key-bindings*`))
				tt.Equal(t, "line-end", keyEval(t, fmt.Sprintf(`(repl-key-binding "%s")`, k.name)))
			})
		}
	}
}

func TestKeyNamedAliasReplacesSameEntry(t *testing.T) {
	withKeyBindings(t, `(progn (repl-bind-key "<prior>" 'line-end) (repl-bind-key "<pageup>" 'line-begin))`)
	tt.Equal(t, `(("<prior>" . line-begin))`, keyEval(t, `*repl-key-bindings*`))
}

func TestKeyNamedDefaults(t *testing.T) {
	// repl-key-binding with a name reports the default of the first encoding.
	withKeyBindings(t, "")
	for _, x := range []struct{ key, action string }{
		{key: "<up>", action: "previous-line"},
		{key: "<down>", action: "next-line"},
		{key: "<right>", action: "forward-char"},
		{key: "<left>", action: "back-char"},
		{key: "S-<tab>", action: "shift-tab"},
		{key: "<backtab>", action: "shift-tab"},
		{key: "<home>", action: "nil"},
		{key: "<end>", action: "nil"},
		{key: "<delete>", action: "nil"},
		{key: "<f5>", action: "nil"},
		{key: "C-<right>", action: "nil"},
	} {
		tt.Equal(t, x.action, keyEval(t, fmt.Sprintf(`(repl-key-binding "%s")`, x.key)), x.key)
	}
}

func TestKeyNamedArrowOverridesDefault(t *testing.T) {
	// Binding <right> replaces the default right arrow and the ESC O form.
	withKeyBindings(t, `(repl-bind-key "<right>" 'line-end)`)
	keyEdSeq(t, "abc\x01\x1b[C", "<set-cursor 2:3>", "<set-cursor 2:6>")
}

func TestKeyNamedUnbindRestoresArrow(t *testing.T) {
	withKeyBindings(t, `(progn (repl-bind-key "<right>" 'line-end) (repl-unbind-key "<right>"))`)
	tt.Equal(t, "forward-char", keyEval(t, `(repl-key-binding "<right>")`))
	tt.Equal(t, "nil", keyEval(t, `(repl-key-binding "M-OC")`))
	keyEdSeq(t, "abc\x01\x1b[C", "<set-cursor 2:3>", "<set-cursor 2:4>")
}

// D2: modifiers on names.

var keyModified = []struct {
	name string
	seq  string
}{
	{name: "C-<up>", seq: "\x1b[1;5A"},
	{name: "C-<down>", seq: "\x1b[1;5B"},
	{name: "C-<right>", seq: "\x1b[1;5C"},
	{name: "C-<left>", seq: "\x1b[1;5D"},
	{name: "M-<up>", seq: "\x1b[1;3A"},
	{name: "M-<down>", seq: "\x1b[1;3B"},
	{name: "M-<right>", seq: "\x1b[1;3C"},
	{name: "M-<left>", seq: "\x1b[1;3D"},
	{name: "C-<home>", seq: "\x1b[1;5H"},
	{name: "C-<end>", seq: "\x1b[1;5F"},
	{name: "M-<home>", seq: "\x1b[1;3H"},
	{name: "M-<end>", seq: "\x1b[1;3F"},
	{name: "C-<f1>", seq: "\x1b[1;5P"},
	{name: "C-<f4>", seq: "\x1b[1;5S"},
	{name: "M-<f2>", seq: "\x1b[1;3Q"},
	{name: "M-<f3>", seq: "\x1b[1;3R"},
	{name: "C-<delete>", seq: "\x1b[3;5~"},
}

func TestKeyNamedModifiedWorks(t *testing.T) {
	for _, k := range keyModified {
		t.Run(k.name, func(t *testing.T) {
			withKeyBindings(t, fmt.Sprintf(`(repl-bind-key "%s" 'line-begin)`, k.name))
			tt.Equal(t, fmt.Sprintf(`(("%s" . line-begin))`, k.name), keyEval(t, `*repl-key-bindings*`))
			tt.Equal(t, "line-begin", keyEval(t, fmt.Sprintf(`(repl-key-binding "M-%s")`, k.seq[1:])))
			keyEdTest(t, keyPressMovesToStart(k.seq))
		})
	}
}

func TestKeyNamedModifiedBindsOnlyCSIForm(t *testing.T) {
	// A modified name does not bind the unmodified or ESC O forms.
	withKeyBindings(t, `(progn (repl-bind-key "C-<up>" 'line-end) (repl-bind-key "C-<home>" 'line-end))`)
	tt.Equal(t, "previous-line", keyEval(t, `(repl-key-binding "<up>")`))
	tt.Equal(t, "nil", keyEval(t, `(repl-key-binding "M-OA")`))
	tt.Equal(t, "nil", keyEval(t, `(repl-key-binding "<home>")`))
	tt.Equal(t, "nil", keyEval(t, `(repl-key-binding "M-OH")`))
	tt.Equal(t, "nil", keyEval(t, `(repl-key-binding "M-[1~")`))
}

func TestKeyNamedModifiedRefused(t *testing.T) {
	// Combinations whose prefix the editor doesn't know are refused, naming
	// the key as written.
	for _, key := range []string{
		"S-<right>", "C-M-<up>", "M-C-<up>", "C-<prior>", "C-S-<left>", "M-<delete>",
		"S-<home>", "C-<f5>", "S-<f1>", "C-<insert>",
	} {
		t.Run(key, func(t *testing.T) {
			withKeyBindings(t, "")
			p := keyError(t, fmt.Sprintf(`(repl-bind-key "%s" 'line-end)`, key))
			// The key is named in canonical form, as it would be stored
			// (M-C-<up> is reported as C-M-<up>).
			named := regexp.QuoteMeta(key)
			if key == "M-C-<up>" {
				named = regexp.QuoteMeta("C-M-<up>")
			}
			tt.Equal(t, "/^key "+named+" can not be bound, .* is not a key prefix/", p.Error())
			tt.Equal(t, "nil", keyEval(t, `*repl-key-bindings*`))
		})
	}
}

func TestKeyNamedModifiedRefusedMessageExample(t *testing.T) {
	withKeyBindings(t, "")
	p := keyError(t, `(repl-bind-key "S-<right>" 'line-end)`)
	tt.Equal(t, "key S-<right> can not be bound, M-[1;2 is not a key prefix the editor knows", p.Error())
}

// D3: parsing.

func TestKeyNamedCaseInsensitive(t *testing.T) {
	for _, x := range []struct{ key, stored string }{
		{key: "<Home>", stored: "<home>"},
		{key: "<HOME>", stored: "<home>"},
		{key: "<PageUp>", stored: "<prior>"},
		{key: "<PAGEDOWN>", stored: "<next>"},
		{key: "<F5>", stored: "<f5>"},
		{key: "S-<Tab>", stored: "S-<tab>"},
		{key: "<BackTab>", stored: "S-<tab>"},
		{key: "C-<Right>", stored: "C-<right>"},
	} {
		t.Run(x.key, func(t *testing.T) {
			withKeyBindings(t, fmt.Sprintf(`(repl-bind-key "%s" 'line-end)`, x.key))
			tt.Equal(t, fmt.Sprintf(`(("%s" . line-end))`, x.stored), keyEval(t, `*repl-key-bindings*`))
		})
	}
}

func TestKeyNamedModifierOrderCanonical(t *testing.T) {
	// M-C-<up> is refused (no prefix) but the modifier order does not matter
	// for parsing: both spellings give the same refusal reason.
	withKeyBindings(t, "")
	p1 := keyError(t, `(repl-bind-key "M-C-<up>" 'line-end)`)
	p2 := keyError(t, `(repl-bind-key "C-M-<up>" 'line-end)`)
	tt.Equal(t, "/^key C-M-<up> can not be bound, M-\\[1;7 is not a key prefix/", p1.Error())
	tt.Equal(t, "/^key C-M-<up> can not be bound, M-\\[1;7 is not a key prefix/", p2.Error())
}

func TestKeyNamedLiteralAngle(t *testing.T) {
	// A string that is not modifiers plus a whole <name> keeps the literal
	// reading: M-< is Alt-<.
	withKeyBindings(t, `(repl-bind-key "M-<" 'line-begin)`)
	tt.Equal(t, `(("M-<" . line-begin))`, keyEval(t, `*repl-key-bindings*`))
	keyEdTest(t, keyPressMovesToStart("\x1b<"))
}

func TestKeyNamedCaseVariantsSameEntry(t *testing.T) {
	withKeyBindings(t, `(progn (repl-bind-key "<home>" 'line-end) (repl-bind-key "<HOME>" 'line-begin))`)
	tt.Equal(t, `(("<home>" . line-begin))`, keyEval(t, `*repl-key-bindings*`))
}

func TestKeyNamedUnknown(t *testing.T) {
	for _, key := range []string{
		"<foo>", "<f0>", "<f13>", "<tab>", "<return>", "<backspace>", "C-<foo>", "M-<pgup>",
		"C-C-<up>", "C-S-<tab>", "M-S-<tab>",
	} {
		t.Run(key, func(t *testing.T) {
			withKeyBindings(t, "")
			for _, form := range []string{
				`(repl-bind-key "%s" 'line-end)`,
				`(repl-key-binding "%s")`,
				`(repl-unbind-key "%s")`,
			} {
				src := fmt.Sprintf(form, key)
				p := keyError(t, src)
				tt.Equal(t, "/^invalid key/", p.Error(), src)
			}
		})
	}
}

// D4: storage.

func TestKeyNamedStoredAsName(t *testing.T) {
	withKeyBindings(t, `(repl-bind-key "C-<right>" 'forward-word)`)
	tt.Equal(t, `(("C-<right>" . forward-word))`, keyEval(t, `*repl-key-bindings*`))
}

func TestKeyNamedUnbindRemovesAllEncodings(t *testing.T) {
	withKeyBindings(t, `(repl-bind-key "<home>" 'line-begin)`)
	tt.Equal(t, "t", keyEval(t, `(repl-unbind-key "<home>")`))
	tt.Equal(t, "nil", keyEval(t, `*repl-key-bindings*`))
	for _, raw := range []string{"M-[H", "M-[1~", "M-OH"} {
		tt.Equal(t, "nil", keyEval(t, fmt.Sprintf(`(repl-key-binding "%s")`, raw)), raw)
	}
	keyEdTest(t, []any{
		startSteps,
		provide("\x1b[1~"),
		until("/key <home> is undefined/"),
	})
}

func TestKeyNamedUnbindWithAlias(t *testing.T) {
	withKeyBindings(t, `(repl-bind-key "<prior>" 'line-begin)`)
	tt.Equal(t, "t", keyEval(t, `(repl-unbind-key "<PageUp>")`))
	tt.Equal(t, "nil", keyEval(t, `*repl-key-bindings*`))
}

func TestKeyNamedRawBindsOnlyThatEncoding(t *testing.T) {
	withKeyBindings(t, `(repl-bind-key "M-[H" 'line-begin)`)
	tt.Equal(t, `(("M-[H" . line-begin))`, keyEval(t, `*repl-key-bindings*`))
	tt.Equal(t, "line-begin", keyEval(t, `(repl-key-binding "M-[H")`))
	tt.Equal(t, "nil", keyEval(t, `(repl-key-binding "M-[1~")`))
	tt.Equal(t, "nil", keyEval(t, `(repl-key-binding "M-OH")`))
	keyEdTest(t, []any{
		startSteps,
		provide("\x1b[1~"),
		until("/key <home> is undefined/"),
	})
}

func TestKeyNamedLaterEntryWins(t *testing.T) {
	withKeyBindings(t, `(setq *repl-key-bindings* '(("<home>" . line-begin) ("M-[1~" . line-end)))`)
	tt.Equal(t, "line-end", keyEval(t, `(repl-key-binding "M-[1~")`))
	tt.Equal(t, "line-begin", keyEval(t, `(repl-key-binding "M-[H")`))
	tt.Equal(t, "line-begin", keyEval(t, `(repl-key-binding "M-OH")`))
}

func TestKeyNamedLaterNameWins(t *testing.T) {
	withKeyBindings(t, `(setq *repl-key-bindings* '(("M-[1~" . line-end) ("<home>" . line-begin)))`)
	tt.Equal(t, "line-begin", keyEval(t, `(repl-key-binding "M-[1~")`))
	tt.Equal(t, "line-begin", keyEval(t, `(repl-key-binding "M-[H")`))
}

func TestKeyNamedUnbindNameKeepsRawEntry(t *testing.T) {
	withKeyBindings(t, `(progn (repl-bind-key "<home>" 'line-begin) (repl-bind-key "M-[1~" 'line-end))`)
	tt.Equal(t, `(("<home>" . line-begin) ("M-[1~" . line-end))`, keyEval(t, `*repl-key-bindings*`))
	_ = keyEval(t, `(repl-unbind-key "<home>")`)
	tt.Equal(t, `(("M-[1~" . line-end))`, keyEval(t, `*repl-key-bindings*`))
	tt.Equal(t, "line-end", keyEval(t, `(repl-key-binding "M-[1~")`))
	tt.Equal(t, "nil", keyEval(t, `(repl-key-binding "M-[H")`))
}

func TestKeyNamedDisableName(t *testing.T) {
	// A name bound to nil disables every encoding, including the defaults.
	withKeyBindings(t, `(repl-bind-key "<up>" nil)`)
	tt.Equal(t, "nil", keyEval(t, `(repl-key-binding "M-[A")`))
	keyEdTest(t, []any{
		startSteps,
		provide("\x1b[A"),
		until("/key <up> is undefined/"),
	})
}

func TestKeyNamedPersistRoundTrip(t *testing.T) {
	keyConfigSetup(t)
	_ = keyEval(t, `(progn (repl-bind-key "C-<right>" 'forward-word) (repl-bind-key "<PageUp>" 'line-begin))`)
	tt.Equal(t, `(setq *repl-key-bindings* '(("C-<right>" . forward-word) ("<prior>" . line-begin)))`,
		keyBindingsLine(t, readKeyConfig(t)))
}

// D5: display.

func TestKeyNamedUndefinedMessages(t *testing.T) {
	for _, x := range []struct{ seq, shown string }{
		{seq: "\x1b[15~", shown: "<f5>"},
		{seq: "\x1b[1;5C", shown: "C-<right>"},
		{seq: "\x1b[1;3D", shown: "M-<left>"},
		{seq: "\x1b[1;2C", shown: "S-<right>"},
		{seq: "\x1b[1;7A", shown: "C-M-<up>"},
		{seq: "\x1b[H", shown: "<home>"},
		{seq: "\x1b[1~", shown: "<home>"},
		{seq: "\x1bOH", shown: "<home>"},
		{seq: "\x1b[5~", shown: "<prior>"},
		{seq: "\x1b[6~", shown: "<next>"},
		{seq: "\x1bOP", shown: "<f1>"},
		{seq: "\x1b[3;5~", shown: "C-<delete>"},
		{seq: "\x1b[1;5E", shown: "M-[1;5E"},
		{seq: "\x1b[Q", shown: "M-[Q"},
	} {
		t.Run(x.shown, func(t *testing.T) {
			withKeyBindings(t, "")
			keyEdTest(t, []any{
				startSteps,
				provide(x.seq),
				until("<inverse>"),
				expect("  "),
				expect("/^key " + regexp.QuoteMeta(x.shown) + " is undefined. sequence: /"),
			})
		})
	}
}

func TestKeyNamedHelpShowsStoredEntries(t *testing.T) {
	withKeyBindings(t, `(progn (repl-bind-key "<Home>" 'line-end) (repl-bind-key "M-[1;5C" 'forward-word))`)
	keyEdTest(t, []any{
		startSteps,
		provide("\x08"),
		until("/Your bindings/"),
		until("<home>"),
		until("M-[1;5C"),
		until(keyBoxEnd),
		provide("\x01"),
	})
}

func TestKeyNamedMessageNameRoundTrip(t *testing.T) {
	// The name shown for an undefined sequence binds that sequence.
	for _, x := range []struct{ seq, shown string }{
		{seq: "\x1b[15~", shown: "<f5>"},
		{seq: "\x1b[1~", shown: "<home>"},
		{seq: "\x1bOA", shown: "<up>"},
		{seq: "\x1b[1;5C", shown: "C-<right>"},
		{seq: "\x1b[1;3H", shown: "M-<home>"},
	} {
		t.Run(x.shown, func(t *testing.T) {
			withKeyBindings(t, fmt.Sprintf(`(repl-bind-key "%s" 'line-begin)`, x.shown))
			keyEdTest(t, keyPressMovesToStart(x.seq))
		})
	}
}

// D6: docs.

func TestKeyNamedHelpEditMentionsNames(t *testing.T) {
	var out strings.Builder
	scope := slip.NewScope()
	scope.Let("*standard-output*", &slip.OutputStream{Writer: &out})
	(&sliptest.Function{
		Scope:  scope,
		Source: `(help 'edit)`,
		Expect: "",
	}).Test(t)
	text := out.String()
	for _, name := range []string{"<home>", "<f1>", "<f12>", "<prior>", "<pageup>", "S-<tab>", "C-<right>", "repl-bind-key"} {
		tt.Equal(t, true, strings.Contains(text, name), "(help 'edit) does not mention %s", name)
	}
}
