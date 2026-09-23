// Copyright (c) 2026, Peter Ohler, All rights reserved.

package repl

import (
	"os"
	"path/filepath"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/repl"
)

// Sanity check of the keyEdTest harness against long standing behavior.
func TestKeyEdHarnessDefaultKeys(t *testing.T) {
	keyEdSeq(t, "abc\x01\x05", "a", "b", "c", "<set-cursor 2:3>", "<set-cursor 2:6>")
}

func TestKeyEdUserBindingOnUnboundKey(t *testing.T) {
	withKeyBindings(t, `(repl-bind-key "C-l" 'line-end)`)
	keyEdSeq(t, "abc\x01\x0c", "a", "b", "c", "<set-cursor 2:3>", "<set-cursor 2:6>")
}

func TestKeyEdUserBindingReplacesDefault(t *testing.T) {
	withKeyBindings(t, `(repl-bind-key "C-e" 'line-begin)`)
	keyEdSeq(t, "abc\x05", "a", "b", "c", "<set-cursor 2:3>")
}

func TestKeyEdUserBindingSetThroughVariable(t *testing.T) {
	withKeyBindings(t, `(setq *repl-key-bindings* '(("C-l" . line-begin)))`)
	keyEdSeq(t, "abc\x0c", "a", "b", "c", "<set-cursor 2:3>")
}

func TestKeyEdDisabledKeyIsUndefined(t *testing.T) {
	withKeyBindings(t, `(repl-bind-key "C-a" nil)`)
	keyEdTest(t, []any{
		startSteps,
		provide("abc"),
		until("c"),
		provide("\x01"),
		until("<inverse>"),
		expect("  "),
		expect("/key C-a is undefined. sequence: \\[\\]byte{0x1} */"),
		expect("<normal>"),
	})
}

func TestKeyEdUnbindRestoresDefault(t *testing.T) {
	withKeyBindings(t, `(progn (repl-bind-key "C-a" nil) (repl-unbind-key "C-a"))`)
	keyEdSeq(t, "abc\x01", "a", "b", "c", "<set-cursor 2:3>")
}

func TestKeyEdUserMetaKeyOneRead(t *testing.T) {
	// M-f is forward-word by default (would land on 2:6); rebound it goes to
	// the line end.
	withKeyBindings(t, `(repl-bind-key "M-f" 'line-end)`)
	keyEdSeq(t, "abc def\x01\x1bf", "<set-cursor 2:3>", "<set-cursor 2:10>")
}

func TestKeyEdUserMetaKeySplitReads(t *testing.T) {
	// Decision 11: ESC then f typed separately still matches the user M-f.
	withKeyBindings(t, `(repl-bind-key "M-f" 'line-end)`)
	keyEdTest(t, []any{
		startSteps,
		provide("abc def\x01"),
		until("<set-cursor 2:3>"),
		provide("\x1b"),
		provide("f"),
		until("<set-cursor 2:10>"),
	})
}

func TestKeyEdUserLongKeySplitReads(t *testing.T) {
	// A user binding several bytes deep, delivered one read per piece.
	withKeyBindings(t, `(repl-bind-key "M-[1;5C" 'line-end)`)
	keyEdTest(t, []any{
		startSteps,
		provide("abc def\x01"),
		until("<set-cursor 2:3>"),
		provide("\x1b["),
		provide("1;5"),
		provide("C"),
		until("<set-cursor 2:10>"),
	})
}

func TestKeyEdHeldPrefixFallsThroughToDefault(t *testing.T) {
	// M-x is a user key so ESC is held in the user layer; ESC f is not a user
	// key so the held ESC plus f must reach the default forward-word.
	withKeyBindings(t, `(repl-bind-key "M-x" 'line-begin)`)
	keyEdSeq(t, "abc def\x01\x1bf", "<set-cursor 2:3>", "<set-cursor 2:6>")
}

func TestKeyEdHeldPrefixFallsThroughSplitReads(t *testing.T) {
	withKeyBindings(t, `(repl-bind-key "M-x" 'line-begin)`)
	keyEdTest(t, []any{
		startSteps,
		provide("abc def\x01"),
		until("<set-cursor 2:3>"),
		provide("\x1b"),
		provide("f"),
		until("<set-cursor 2:6>"),
	})
}

func TestKeyEdHeldLongPrefixFallsThroughToArrow(t *testing.T) {
	// ESC [ is held for the user M-[1;5C but ESC [ D is the default left
	// arrow.
	withKeyBindings(t, `(repl-bind-key "M-[1;5C" 'line-end)`)
	keyEdSeq(t, "ab\x1b[D", "a", "b", "<set-cursor 2:4>")
}

func TestKeyEdSiblingUserKeys(t *testing.T) {
	withKeyBindings(t, `(progn (repl-bind-key "M-x" 'line-begin) (repl-bind-key "M-y" 'line-end))`)
	keyEdSeq(t, "abc\x1bx\x1by", "a", "b", "c", "<set-cursor 2:3>", "<set-cursor 2:6>")
}

func TestKeyEdUserKeyAfterClearingBindings(t *testing.T) {
	// Setting the variable to nil removes the user layer entirely.
	withKeyBindings(t, `(progn (repl-bind-key "C-a" 'line-end) (setq *repl-key-bindings* nil))`)
	keyEdSeq(t, "abc\x01", "a", "b", "c", "<set-cursor 2:3>")
}

func TestKeyEdLoneEscClosesCompletions(t *testing.T) {
	// A lone ESC while completions are shown still closes them even when ESC
	// is a prefix in the user layer, so the following n is a plain n and not
	// M-n.
	withKeyBindings(t, `(repl-bind-key "M-x" 'line-begin)`)
	keyEdTest(t, []any{
		startSteps,
		provide("*pri"),
		until("i"),
		provide("\t"),
		until("nt-"),
		provide("\t"),
		until("/┕━━━━━━━━━━━━━━*━━━━━━━━━━━━┛/"),
		provide("\x1b"),
		provide("n"),
		until("n"),
	})
}

func TestKeyEdLoneEscClosesCompletionsDefault(t *testing.T) {
	// Control for TestKeyEdLoneEscClosesCompletions without user bindings.
	keyEdTest(t, []any{
		startSteps,
		provide("*pri"),
		until("i"),
		provide("\t"),
		until("nt-"),
		provide("\t"),
		until("/┕━━━━━━━━━━━━━━*━━━━━━━━━━━━┛/"),
		provide("\x1b"),
		provide("n"),
		until("n"),
	})
}

var keyHistorySetup = []any{
	startSteps,
	provide("\"a\"\r"),
	until("▶ "),
	expect("<normal>"),
	provide("\"b\"\r"),
	until("▶ "),
	expect("<normal>"),
	provide("\"c\"\r"),
	until("▶ "),
	expect("<normal>"),
}

func TestKeyEdRebindHistoryBack(t *testing.T) {
	// The first C-l enters history through the tables, the next ones must be
	// recognized by historyOverride through the user layer.
	withKeyBindings(t, `(repl-bind-key "C-l" 'history-back)`)
	keyEdTest(t, []any{
		keyHistorySetup,
		provide("\x0c"),
		until("\"c\""),
		until("/<set-cursor .*>/"),
		provide("\x0c"),
		until("\"b\""),
		until("/<set-cursor .*>/"),
		provide("\x0c"),
		until("\"a\""),
		until("/<set-cursor .*>/"),
	})
}

func TestKeyEdRebindHistoryForward(t *testing.T) {
	withKeyBindings(t, `(repl-bind-key "C-l" 'history-forward)`)
	keyEdTest(t, []any{
		keyHistorySetup,
		provide("\x1bv"),
		until("\"c\""),
		until("/<set-cursor .*>/"),
		provide("\x1bv"),
		until("\"b\""),
		until("/<set-cursor .*>/"),
		provide("\x0c"),
		until("\"c\""),
		until("/<set-cursor .*>/"),
	})
}

func TestKeyEdDisabledKeyEndsHistory(t *testing.T) {
	// C-p normally steps back in history once history is active. Disabled,
	// the history override must treat it as a non-history key so the key
	// falls through to the (now undefined) binding.
	withKeyBindings(t, `(repl-bind-key "C-p" nil)`)
	keyEdTest(t, []any{
		keyHistorySetup,
		provide("\x1bv"),
		until("\"c\""),
		until("/<set-cursor .*>/"),
		provide("\x10"),
		untilWithout{target: "/key C-p is undefined/", forbid: `"b"`},
	})
}

func TestKeyEdRebindSearchHistoryBack(t *testing.T) {
	withKeyBindings(t, `(repl-bind-key "C-l" 'search-history-back)`)
	keyEdTest(t, []any{
		keyHistorySetup,
		provide("\x0c"),
		until("/search backwards:   /"),
		until("/<set-cursor .*>/"),
		provide("b"),
		until("\"b\""),
		until("/search backwards: b/"),
	})
}

func TestKeyEdCompletionUserForwardKey(t *testing.T) {
	withKeyBindings(t, `(repl-bind-key "C-l" 'forward-char)`)
	keyEdTest(t, []any{
		startSteps,
		provide("*pri"),
		until("i"),
		provide("\t"),
		until("nt-"),
		provide("\t"),
		until("/┕━━━━━━━━━━━━━━*━━━━━━━━━━━━┛/"),
		provide("\t"),
		until("<inverse>"),
		expect("*print-ansi*"),
		until("/┕━━━━━━━━━━━━━━*━━━━━━━━━━━━┛/"),
		comment("press C-l bound to forward-char"),
		provide("\x0c"),
		until("<inverse>"),
		expect("*print-array*"),
	})
}

func TestKeyEdCompletionUserNextLineKey(t *testing.T) {
	withKeyBindings(t, `(repl-bind-key "C-l" 'next-line)`)
	keyEdTest(t, []any{
		startSteps,
		provide("*pri"),
		until("i"),
		provide("\t"),
		until("nt-"),
		provide("\t"),
		until("/┕━━━━━━━━━━━━━━*━━━━━━━━━━━━┛/"),
		provide("\t"),
		until("<inverse>"),
		expect("*print-ansi*"),
		until("/┕━━━━━━━━━━━━━━*━━━━━━━━━━━━┛/"),
		provide("\x0c"),
		until("<inverse>"),
		expect("*print-case*"),
	})
}

func TestKeyEdCompletionDisabledForwardKey(t *testing.T) {
	// C-f disabled is no longer a completion key; the completions close and
	// the key reports undefined.
	withKeyBindings(t, `(repl-bind-key "C-f" nil)`)
	keyEdTest(t, []any{
		startSteps,
		provide("*pri"),
		until("i"),
		provide("\t"),
		until("nt-"),
		provide("\t"),
		until("/┕━━━━━━━━━━━━━━*━━━━━━━━━━━━┛/"),
		provide("\t"),
		until("<inverse>"),
		expect("*print-ansi*"),
		until("/┕━━━━━━━━━━━━━━*━━━━━━━━━━━━┛/"),
		provide("\x06"),
		untilWithout{target: "/key C-f is undefined/", forbid: "*print-array*"},
	})
}

// Keys that are bindable but have no default (addendum C), bound here to the
// actions they were designed for.

func TestKeyEdHomeCSI(t *testing.T) {
	withKeyBindings(t, `(repl-bind-key "M-[H" 'line-begin)`)
	keyEdSeq(t, "abc\x1b[H", "a", "b", "c", "<set-cursor 2:3>")
}

func TestKeyEdEndCSI(t *testing.T) {
	withKeyBindings(t, `(repl-bind-key "M-[F" 'line-end)`)
	keyEdSeq(t, "abc\x01\x1b[F", "<set-cursor 2:3>", "<set-cursor 2:6>")
}

func TestKeyEdHomeTilde(t *testing.T) {
	withKeyBindings(t, `(repl-bind-key "M-[1~" 'line-begin)`)
	keyEdSeq(t, "abc\x1b[1~", "a", "b", "c", "<set-cursor 2:3>")
}

func TestKeyEdEndTilde(t *testing.T) {
	withKeyBindings(t, `(repl-bind-key "M-[4~" 'line-end)`)
	keyEdSeq(t, "abc\x01\x1b[4~", "<set-cursor 2:3>", "<set-cursor 2:6>")
}

func TestKeyEdCtrlDelete(t *testing.T) {
	withKeyBindings(t, `(repl-bind-key "M-[3;5~" 'delete-word)`)
	keyEdSeq(t, "abc def\x01\x1b[3;5~", "<set-cursor 2:3>", " def", "<set-cursor 2:3>")
}

func TestKeyEdCtrlUpFormBegin(t *testing.T) {
	withKeyBindings(t, `(repl-bind-key "M-[1;5A" 'form-begin)`)
	keyEdSeq(t, "ab\ncd\x1b[1;5A", "a", "b", "c", "d", "<set-cursor 2:3>")
}

func TestKeyEdAltUpFormBegin(t *testing.T) {
	withKeyBindings(t, `(repl-bind-key "M-[1;3A" 'form-begin)`)
	keyEdSeq(t, "ab\ncd\x1b[1;3A", "a", "b", "c", "d", "<set-cursor 2:3>")
}

func TestKeyEdCtrlUpFormBeginSingleLine(t *testing.T) {
	withKeyBindings(t, `(repl-bind-key "M-[1;5A" 'form-begin)`)
	keyEdSeq(t, "abc\x1b[1;5A", "a", "b", "c", "<set-cursor 2:3>")
}

func TestKeyEdCtrlDownFormEnd(t *testing.T) {
	// Up to line one, to its start, then Ctrl-Down to the end of the last
	// line.
	withKeyBindings(t, `(repl-bind-key "M-[1;5B" 'form-end)`)
	keyEdSeq(t, "ab\ncd\x10\x01\x1b[1;5B", "a", "b", "c", "d", "<set-cursor 2:3>", "<set-cursor 3:5>")
}

func TestKeyEdAltDownFormEnd(t *testing.T) {
	withKeyBindings(t, `(repl-bind-key "M-[1;3B" 'form-end)`)
	keyEdSeq(t, "ab\ncd\x10\x01\x1b[1;3B", "a", "b", "c", "d", "<set-cursor 2:3>", "<set-cursor 3:5>")
}

func TestKeyEdCtrlDownFormEndLongLastLine(t *testing.T) {
	// Form end goes to the end of the last line, not the same column.
	withKeyBindings(t, `(repl-bind-key "M-[1;5B" 'form-end)`)
	keyEdSeq(t, "a\nbcde\x10\x01\x1b[1;5B", "<set-cursor 2:3>", "<set-cursor 3:7>")
}

func TestKeyEdStillUnboundModifiedArrow(t *testing.T) {
	withKeyBindings(t, "")
	keyEdTest(t, []any{
		startSteps,
		provide("\x1b[1;5E"),
		until("<inverse>"),
		expect("  "),
		expect("/key M-\\[1;5E is undefined. sequence: \\[\\]byte{.+} */"),
		expect("<normal>"),
	})
}

func TestKeyEdUndefinedMessageUsesKeyName(t *testing.T) {
	withKeyBindings(t, "")
	keyEdTest(t, []any{
		startSteps,
		provide("\x0c"),
		until("<inverse>"),
		expect("  "),
		expect("/key C-l is undefined. sequence: \\[\\]byte{0xc} */"),
		expect("<normal>"),
	})
}

func TestKeyEdRebindUserKey(t *testing.T) {
	// Binding a bound key again replaces its action.
	withKeyBindings(t, `(progn (repl-bind-key "M-[H" 'line-begin) (repl-bind-key "M-[H" 'line-end))`)
	keyEdSeq(t, "abc\x01\x1b[H", "<set-cursor 2:3>", "<set-cursor 2:6>")
}

// After a user bound key the editor is back in the top mode,
// so a following x is inserted rather than read as part of a sequence.
func TestKeyEdBackToTopModeAfterKey(t *testing.T) {
	for _, x := range []struct {
		name  string
		setup string
		key   string
	}{
		{name: "M-[H", setup: `(repl-bind-key "M-[H" 'line-begin)`, key: "\x1b[H"},
		{name: "M-[F", setup: `(repl-bind-key "M-[F" 'line-end)`, key: "\x1b[F"},
		{name: "M-[1~", setup: `(repl-bind-key "M-[1~" 'line-begin)`, key: "\x1b[1~"},
		{name: "M-[4~", setup: `(repl-bind-key "M-[4~" 'line-end)`, key: "\x1b[4~"},
		{name: "M-[3;5~", setup: `(repl-bind-key "M-[3;5~" 'delete-word)`, key: "\x1b[3;5~"},
		{name: "M-[1;5A", setup: `(repl-bind-key "M-[1;5A" 'form-begin)`, key: "\x1b[1;5A"},
		{name: "M-[1;3A", setup: `(repl-bind-key "M-[1;3A" 'form-begin)`, key: "\x1b[1;3A"},
		{name: "M-[1;5B", setup: `(repl-bind-key "M-[1;5B" 'form-end)`, key: "\x1b[1;5B"},
		{name: "M-[1;3B", setup: `(repl-bind-key "M-[1;3B" 'form-end)`, key: "\x1b[1;3B"},
		{name: "M-[1;5C", setup: `(repl-bind-key "M-[1;5C" 'forward-word)`, key: "\x1b[1;5C"},
		{name: "M-[1;3D", setup: `(repl-bind-key "M-[1;3D" 'back-word)`, key: "\x1b[1;3D"},
		{name: "user C-l", setup: `(repl-bind-key "C-l" 'line-begin)`, key: "\x0c"},
		{name: "user M-x", setup: `(repl-bind-key "M-x" 'line-begin)`, key: "\x1bx"},
		{name: "user M-[1;5C", setup: `(repl-bind-key "M-[1;5C" 'line-begin)`, key: "\x1b[1;5C"},
		{name: "user M-[H", setup: `(repl-bind-key "M-[H" 'line-end)`, key: "\x1b[H"},
		{name: "held M-f", setup: `(repl-bind-key "M-x" 'line-begin)`, key: "\x1bf"},
	} {
		t.Run(x.name, func(t *testing.T) {
			withKeyBindings(t, x.setup)
			keyEdTest(t, []any{
				startSteps,
				provide("abc"),
				until("c"),
				provide(x.key),
				provide("x"),
				untilWithout{target: "/x/", forbid: "/undefined/"},
			})
		})
	}
}

// Addendum A item 1: a key typed in the same read right after a bound
// terminal key is inserted, not taken as part of an escape sequence.
func TestKeyEdHomeThenInsertSameRead(t *testing.T) {
	withKeyBindings(t, `(repl-bind-key "M-[H" 'line-begin)`)
	keyEdTest(t, []any{
		startSteps,
		provide("abc"),
		until("c"),
		provide("\x1b[Ha"),
		untilWithout{target: "aabc", forbid: "/undefined/"},
	})
}

func TestKeyEdHomeTildeThenInsertSameRead(t *testing.T) {
	withKeyBindings(t, `(repl-bind-key "M-[1~" 'line-begin)`)
	keyEdTest(t, []any{
		startSteps,
		provide("abc"),
		until("c"),
		provide("\x1b[1~a"),
		untilWithout{target: "aabc", forbid: "/undefined/"},
	})
}

func TestKeyEdEndThenInsertSameRead(t *testing.T) {
	for _, key := range []string{"\x1b[F", "\x1b[4~"} {
		t.Run(key, func(t *testing.T) {
			withKeyBindings(t, `(progn (repl-bind-key "M-[F" 'line-end) (repl-bind-key "M-[4~" 'line-end))`)
			keyEdTest(t, []any{
				startSteps,
				provide("abc"),
				until("c"),
				provide("\x01"),
				until("<set-cursor 2:3>"),
				provide(key + "x"),
				untilWithout{target: "x", forbid: "/undefined/"},
			})
		})
	}
}

func TestKeyEdDescribeThenKey(t *testing.T) {
	withKeyBindings(t, "")
	keyEdTest(t, []any{
		startSteps,
		provide("( "),
		until(" "),
		provide("\x1b/"),
		until("/could not determine what to describe/"),
		provide("x"),
		untilWithout{target: "/x/", forbid: "/undefined/"},
	})
}

// Addendum A item 6: previous-line bound by the user steps back while
// browsing history, like history-back.
func TestKeyEdUserPreviousLineInHistory(t *testing.T) {
	withKeyBindings(t, `(repl-bind-key "C-l" 'previous-line)`)
	keyEdTest(t, []any{
		keyHistorySetup,
		provide("\x1bv"),
		until("\"c\""),
		until("/<set-cursor .*>/"),
		provide("\x0c"),
		until("\"b\""),
		until("/<set-cursor .*>/"),
		provide("\x0c"),
		until("\"a\""),
	})
}

func TestKeyEdUserNextLineInHistory(t *testing.T) {
	withKeyBindings(t, `(repl-bind-key "C-l" 'next-line)`)
	keyEdTest(t, []any{
		keyHistorySetup,
		provide("\x1bv"),
		until("\"c\""),
		until("/<set-cursor .*>/"),
		provide("\x1bv"),
		until("\"b\""),
		until("/<set-cursor .*>/"),
		provide("\x0c"),
		until("\"c\""),
	})
}

// Addendum A item 7: the "dirty" exemption that keeps the completion list up
// for TAB follows the effective action, so a user key bound to tab behaves
// like TAB and the list is not cleared first.
func TestKeyEdUserTabKeepsCompletions(t *testing.T) {
	withKeyBindings(t, `(repl-bind-key "C-l" 'tab)`)
	keyEdTest(t, []any{
		startSteps,
		provide("*pri"),
		until("i"),
		provide("\t"),
		until("nt-"),
		provide("\t"),
		until("/┕━━━━━━━━━━━━━━*━━━━━━━━━━━━┛/"),
		provide("\x0c"),
		untilWithout{target: "<inverse>", forbid: "/<clear-down/"},
		expect("*print-ansi*"),
	})
}

func TestKeyEdTabKeepsCompletionsDefault(t *testing.T) {
	// Control for TestKeyEdUserTabKeepsCompletions using TAB itself.
	withKeyBindings(t, "")
	keyEdTest(t, []any{
		startSteps,
		provide("*pri"),
		until("i"),
		provide("\t"),
		until("nt-"),
		provide("\t"),
		until("/┕━━━━━━━━━━━━━━*━━━━━━━━━━━━┛/"),
		provide("\t"),
		untilWithout{target: "<inverse>", forbid: "/<clear-down/"},
		expect("*print-ansi*"),
	})
}

// Addendum A item 12: the undefined message names the whole key with no mode
// prefix.
func TestKeyEdUndefinedMessageFullKeyName(t *testing.T) {
	for _, x := range []struct {
		key  string
		name string
	}{
		{key: "\x1b[Q", name: `M-\[Q`},
		{key: "\x1bx", name: "M-x"},
		// The rest of the read is part of the reported key; ESC [ 1 ; 2 C is
		// the encoding of S-<right> so it is shown by name (addendum D5).
		{key: "\x1b[1;2C", name: `S-<right>`},
		{key: "\x1b\x1b\x7f", name: "M-M-DEL"},
	} {
		t.Run(x.name, func(t *testing.T) {
			withKeyBindings(t, "")
			keyEdTest(t, []any{
				startSteps,
				provide(x.key),
				until("<inverse>"),
				expect("  "),
				expect("/^key " + x.name + " is undefined. sequence: /"),
			})
		})
	}
}

// Addendum A item 15: the user layer is consulted only in the top mode, so a
// user binding for "A" does not hijack the A of the ESC [ A up arrow.
func TestKeyEdUserLetterDoesNotHijackArrow(t *testing.T) {
	withKeyBindings(t, `(repl-bind-key "A" 'line-end)`)
	keyEdTest(t, []any{
		startSteps,
		provide("ab\ncd"),
		until("d"),
		until("<set-cursor 3:5>"),
		provide("\x1b[A"),
		untilWithout{target: "<set-cursor 2:5>", forbid: "/<set-cursor 3:5>|undefined/"},
	})
}

func TestKeyEdUserLetterDoesNotHijackLeftArrow(t *testing.T) {
	withKeyBindings(t, `(repl-bind-key "D" 'line-end)`)
	keyEdTest(t, []any{
		startSteps,
		provide("abc"),
		until("c"),
		until("<set-cursor 2:6>"),
		provide("\x1b[D"),
		untilWithout{target: "<set-cursor 2:5>", forbid: "/<set-cursor 2:6>|undefined/"},
	})
}

func TestKeyEdUserLetterAtTop(t *testing.T) {
	// The same "A" binding does apply when A is typed on its own.
	withKeyBindings(t, `(repl-bind-key "A" 'line-begin)`)
	keyEdSeq(t, "abcA", "a", "b", "c", "<set-cursor 2:3>")
}

// Help page.

func TestKeyEdHelpShowsUserBindings(t *testing.T) {
	// The exact layout of the "Your bindings" block is not fixed by the spec
	// so only its presence and the entries are checked.
	withKeyBindings(t, `(progn (repl-bind-key "C-l" 'line-end) (repl-bind-key "C-g" nil))`)
	keyEdTest(t, []any{
		startSteps,
		provide("\x08"),
		until("/Your bindings/"),
		until("/C-l/"),
		until("/line-end/"),
		until("/C-g/"),
		until("/undefined/"),
		until("/┕━━━━━━━━━━━━━━*━━━━━━━━━━━━┛/"),
		provide("\x01"),
	})
}

func TestKeyEdHelpUserBindingsAligned(t *testing.T) {
	// Actions in the "Your bindings" block line up after the longest key
	// (M-[1;5C, 7 wide) plus two spaces.
	withKeyBindings(t, `(progn (repl-bind-key "C-l" 'line-end) (repl-bind-key "M-[1;5C" 'forward-word))`)
	keyEdTest(t, []any{
		startSteps,
		provide("\x08"),
		until("/Your bindings/"),
		until("C-l"),
		expect("<normal>"),
		expect("/^ {6}line-end/"),
		until("M-[1;5C"),
		expect("<normal>"),
		expect("/^ {2}forward-word/"),
		until("/┕━━━━━━━━━━━━━━*━━━━━━━━━━━━┛/"),
		provide("\x01"),
	})
}

func TestKeyEdHelpMarksOverriddenDefaultRow(t *testing.T) {
	// The spec suggests appending " *" to default rows whose key has a user
	// binding; any "*" after the C-t row description is accepted.
	withKeyBindings(t, `(repl-bind-key "C-t" 'line-end)`)
	keyEdTest(t, []any{
		startSteps,
		provide("\x08"),
		until("/Your bindings/"),
		comment("the user block row"),
		until("C-t"),
		expect("<normal>"),
		expect("/line-end/"),
		comment("the default row for C-t is marked"),
		until("C-t"),
		expect("<normal>"),
		expect(`/swap characters.*\*/`),
		until("/┕━━━━━━━━━━━━━━*━━━━━━━━━━━━┛/"),
		provide("\x01"),
	})
}

func TestKeyEdHelpWithoutUserBindings(t *testing.T) {
	withKeyBindings(t, "")
	keyEdTest(t, []any{
		startSteps,
		provide("\x08"),
		untilWithout{target: "/┕━━━━━━━━━━━━━━*━━━━━━━━━━━━┛/", forbid: "/Your bindings/"},
		provide("\x01"),
	})
}

// Addendum A item 1: after help and clear-form the next key is handled
// normally.
func TestKeyEdKeyAfterHelp(t *testing.T) {
	withKeyBindings(t, "")
	keyEdTest(t, []any{
		startSteps,
		provide("\x08"),
		until("/┕━━━━━━━━━━━━━━*━━━━━━━━━━━━┛/"),
		provide("x"),
		untilWithout{target: "x", forbid: "/undefined/"},
	})
}

func TestKeyEdKeyAfterClearForm(t *testing.T) {
	withKeyBindings(t, "")
	keyEdTest(t, []any{
		startSteps,
		provide("abc"),
		until("c"),
		provide("\x15"),
		until("▶ "),
		provide("x"),
		untilWithout{target: "x", forbid: "/undefined/"},
	})
}

func TestKeyEdKeyAfterUserHelpUnderEsc(t *testing.T) {
	// help reached through the ESC table via a user binding.
	withKeyBindings(t, `(repl-bind-key "M-x" 'help)`)
	keyEdTest(t, []any{
		startSteps,
		provide("\x1bx"),
		until("/┕━━━━━━━━━━━━━━*━━━━━━━━━━━━┛/"),
		provide("x"),
		untilWithout{target: "x", forbid: "/undefined/"},
	})
}

func TestKeyEdKeyAfterUserClearFormUnderCSI(t *testing.T) {
	withKeyBindings(t, `(repl-bind-key "M-[1;5C" 'clear-form)`)
	keyEdTest(t, []any{
		startSteps,
		provide("abc"),
		until("c"),
		provide("\x1b[1;5C"),
		until("▶ "),
		provide("x"),
		untilWithout{target: "x", forbid: "/undefined/"},
	})
}

// Addendum A item 7: a user key bound to tab scrolls a help page that does
// not fit rather than dismissing it. Scrolling redraws inside the box
// (clear-to-end); dismissing clears below the form (clear-down).
func TestKeyEdUserTabScrollsHelp(t *testing.T) {
	withKeyBindings(t, `(repl-bind-key "C-l" 'tab)`)
	keyEdTestSize(t, 20, 80, []any{
		startSteps,
		provide("\x08"),
		until("/┕━━━━━━━━━━━━━━*━━━━━━━━━━━━┛/"),
		provide("\x0c"),
		untilWithout{target: "<clear-to-end 3:4>", forbid: "/<clear-down/"},
		until("/┕━━━━━━━━━━━━━━*━━━━━━━━━━━━┛/"),
	})
}

func TestKeyEdTabScrollsHelpDefault(t *testing.T) {
	// Control for TestKeyEdUserTabScrollsHelp with TAB itself.
	withKeyBindings(t, "")
	keyEdTestSize(t, 20, 80, []any{
		startSteps,
		provide("\x08"),
		until("/┕━━━━━━━━━━━━━━*━━━━━━━━━━━━┛/"),
		provide("\t"),
		untilWithout{target: "<clear-to-end 3:4>", forbid: "/<clear-down/"},
		until("/┕━━━━━━━━━━━━━━*━━━━━━━━━━━━┛/"),
	})
}

func TestKeyEdUserShiftTabScrollsHelp(t *testing.T) {
	withKeyBindings(t, `(repl-bind-key "C-l" 'shift-tab)`)
	keyEdTestSize(t, 20, 80, []any{
		startSteps,
		provide("\x08"),
		until("/┕━━━━━━━━━━━━━━*━━━━━━━━━━━━┛/"),
		provide("\t"),
		until("/┕━━━━━━━━━━━━━━*━━━━━━━━━━━━┛/"),
		provide("\x0c"),
		untilWithout{target: "<clear-to-end 3:4>", forbid: "/<clear-down/"},
		until("/┕━━━━━━━━━━━━━━*━━━━━━━━━━━━┛/"),
	})
}

func TestKeyEdDisabledTabDismissesHelp(t *testing.T) {
	// With TAB disabled it is no longer exempt, so it dismisses the help.
	withKeyBindings(t, `(repl-bind-key "TAB" nil)`)
	keyEdTestSize(t, 20, 80, []any{
		startSteps,
		provide("\x08"),
		until("/┕━━━━━━━━━━━━━━*━━━━━━━━━━━━┛/"),
		provide("\t"),
		untilWithout{target: "/<clear-down/", forbid: "<clear-to-end 3:4>"},
	})
}

// Addendum A item 15 with history: ESC [ A on a blank form still steps back
// in history when "A" has a user binding.
func TestKeyEdUserLetterDoesNotHijackHistory(t *testing.T) {
	withKeyBindings(t, `(repl-bind-key "A" 'line-end)`)
	keyEdTest(t, []any{
		keyHistorySetup,
		provide("\x1b[A"),
		untilWithout{target: "\"c\"", forbid: "/undefined/"},
		until("/<set-cursor .*>/"),
		provide("\x1b[A"),
		untilWithout{target: "\"b\"", forbid: "/undefined/"},
	})
}

// Addendum A item 5: bindings changed by a form evaluated on close take
// effect for the next key, and the user layer starts from its root.
//
// Holding ESC across the evaluation itself can't be driven from outside: the
// evaluation is started by the closing paren, and the editor drops the rest
// of that read, so an ESC sent with the paren never reaches the user layer.
// This test covers the observable part: the new binding (M-y, under the ESC
// prefix) works right after the eval, with ESC and y in separate reads.
func TestKeyEdBindingChangedByEvalOnClose(t *testing.T) {
	withKeyBindings(t, `(repl-bind-key "M-x" 'line-end)`)
	scope := repl.GetScope()
	orig := scope.Get(slip.Symbol("*repl-eval-on-close*"))
	scope.Set(slip.Symbol("*repl-eval-on-close*"), slip.True)
	defer scope.Set(slip.Symbol("*repl-eval-on-close*"), orig)
	keyEdTest(t, []any{
		startSteps,
		keyChunks(`(repl-bind-key "M-y" 'line-begin)`),
		until("line-begin\n"),
		until("▶ "),
		provide("abc"),
		until("c"),
		until("/<set-cursor \\d+:6>/"),
		provide("\x1b"),
		provide("y"),
		untilWithout{target: "/<set-cursor \\d+:3>/", forbid: "/undefined/"},
		comment("the earlier M-x binding still applies"),
		provide("\x1bx"),
		untilWithout{target: "/<set-cursor \\d+:6>/", forbid: "/undefined/"},
	})
}

func TestKeyEdBindingChangedByEval(t *testing.T) {
	// The same with RET: the replaced layer is used after the eval.
	withKeyBindings(t, `(repl-bind-key "M-x" 'line-end)`)
	keyEdTest(t, []any{
		startSteps,
		keyChunks("(setq *repl-key-bindings* '((\"M-y\" . line-begin)))\r"),
		until("▶ "),
		provide("abc"),
		until("c"),
		until("/<set-cursor \\d+:6>/"),
		provide("\x1b"),
		provide("y"),
		untilWithout{target: "/<set-cursor \\d+:3>/", forbid: "/undefined/"},
		comment("M-x is gone so it is undefined again"),
		provide("\x1bx"),
		until("/key M-x is undefined/"),
	})
}

// lineBegin on a line wider than the terminal: the view is shifted and must
// be redrawn from the start of the line.
func TestKeyEdLineBeginShifted(t *testing.T) {
	for _, x := range []struct {
		name  string
		setup string
		key   string
	}{
		{name: "C-a", key: "\x01"},
		{name: "M-[H", setup: `(repl-bind-key "M-[H" 'line-begin)`, key: "\x1b[H"},
		{name: "user M-x", setup: `(repl-bind-key "M-x" 'line-begin)`, key: "\x1bx"},
	} {
		t.Run(x.name, func(t *testing.T) {
			withKeyBindings(t, x.setup)
			keyEdTestSize(t, 40, 20, []any{
				startSteps,
				keyChunks("abcdefghijklmnopqrstuvwxyz0123"),
				until("/0123$/"),
				provide(x.key),
				untilWithout{target: "/^abcdefghijklmn/", forbid: "/undefined/"},
				until("<set-cursor 2:3>"),
				provide("x"),
				untilWithout{target: "/^xabcdefghijklm/", forbid: "/undefined/"},
			})
		})
	}
}

// editForm (M-C-e) with no external editor configured does nothing and the
// next key is handled normally.
func TestKeyEdEditFormWithoutEditor(t *testing.T) {
	withKeyBindings(t, "")
	t.Setenv("EDITOR", "")
	scope := repl.GetScope()
	orig := scope.Get(slip.Symbol("*repl-external-editor*"))
	scope.Set(slip.Symbol("*repl-external-editor*"), slip.String(""))
	defer scope.Set(slip.Symbol("*repl-external-editor*"), orig)
	keyEdTest(t, []any{
		startSteps,
		provide("abc"),
		until("c"),
		provide("\x1b\x05"),
		provide("x"),
		untilWithout{target: "x", forbid: "/undefined/"},
	})
}

// editForm through a user key under the CSI tables, with an "editor" script
// that rewrites the file. The editor pauses input and drops one pending key
// when it returns, so a throwaway key is sent before the checked one.
func TestKeyEdUserEditFormRunsEditor(t *testing.T) {
	withKeyBindings(t, `(repl-bind-key "M-[1;5E" 'edit-form)`)
	script := "config/key-editor.sh"
	tt.Nil(t, os.WriteFile(script, []byte("#!/bin/sh\necho '(+ 1 2)' > \"$1\"\n"), 0755))
	defer func() { _ = os.Remove(script) }()
	abs, err := filepath.Abs(script)
	tt.Nil(t, err)
	t.Setenv("EDITOR", abs)
	scope := repl.GetScope()
	orig := scope.Get(slip.Symbol("*repl-external-editor*"))
	origFlags := scope.Get(slip.Symbol("*repl-editor-flags*"))
	scope.Set(slip.Symbol("*repl-external-editor*"), slip.String(""))
	scope.Set(slip.Symbol("*repl-editor-flags*"), nil)
	defer func() {
		scope.Set(slip.Symbol("*repl-external-editor*"), orig)
		scope.Set(slip.Symbol("*repl-editor-flags*"), origFlags)
	}()
	keyEdTest(t, []any{
		startSteps,
		provide("abc"),
		until("c"),
		provide("\x1b[1;5E"),
		until("/\\(\\+ 1 2\\)/"),
		provide("z"),
		provide("x"),
		untilWithout{target: "/x/", forbid: "/undefined|z/"},
	})
}

// editForm with *repl-external-editor* and *repl-editor-flags* set: the
// flags are passed before the file name.
func TestKeyEdEditFormExternalEditorFlags(t *testing.T) {
	withKeyBindings(t, "")
	script := "config/key-editor-flags.sh"
	tt.Nil(t, os.WriteFile(script, []byte("#!/bin/sh\n[ \"$1\" = \"-q\" ] || exit 1\necho '(+ 3 4)' > \"$2\"\n"), 0755))
	defer func() { _ = os.Remove(script) }()
	abs, err := filepath.Abs(script)
	tt.Nil(t, err)
	scope := repl.GetScope()
	orig := scope.Get(slip.Symbol("*repl-external-editor*"))
	origFlags := scope.Get(slip.Symbol("*repl-editor-flags*"))
	scope.Set(slip.Symbol("*repl-external-editor*"), slip.String(abs))
	scope.Set(slip.Symbol("*repl-editor-flags*"), slip.List{slip.String("-q")})
	defer func() {
		scope.Set(slip.Symbol("*repl-external-editor*"), orig)
		scope.Set(slip.Symbol("*repl-editor-flags*"), origFlags)
	}()
	keyEdTest(t, []any{
		startSteps,
		provide("abc"),
		until("c"),
		provide("\x1b\x05"),
		until("/\\(\\+ 3 4\\)/"),
		provide("z"),
		provide("x"),
		untilWithout{target: "/x/", forbid: "/undefined|z/"},
	})
}

// TAB arriving in the same read as another TAB while a scrolling help page is
// shown scrolls rather than dismissing it, with no user bindings.
func TestKeyEdTabsInOneReadScrollHelp(t *testing.T) {
	withKeyBindings(t, "")
	keyEdTestSize(t, 20, 80, []any{
		startSteps,
		provide("\x08"),
		until("/┕━━━━━━━━━━━━━━*━━━━━━━━━━━━┛/"),
		provide("\t\t"),
		untilWithout{target: "<clear-to-end 3:4>", forbid: "/<clear-down/"},
		until("/┕━━━━━━━━━━━━━━*━━━━━━━━━━━━┛/"),
	})
}
