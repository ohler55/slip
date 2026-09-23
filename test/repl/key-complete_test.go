// Copyright (c) 2026, Peter Ohler, All rights reserved.

package repl

import (
	"testing"
)

// Completion list navigation (completeOverride). The list for *pri starts
// *print-ansi*, *print-array*, *print-case*, *print-circle* ... and ends
// with *print-right-margin*; moving down from *print-ansi* lands on
// *print-case* and up wraps to *print-readably*.

const keyBoxEnd = "/┕━━━━━━━━━━━━━━*━━━━━━━━━━━━┛/"

// keyChoices types *pri and opens the completion list without a pick.
var keyChoices = []any{
	startSteps,
	provide("*pri"),
	until("i"),
	provide("\t"),
	until("nt-"),
	provide("\t"),
	until(keyBoxEnd),
}

// keyFirstPick opens the list and highlights *print-ansi*.
var keyFirstPick = []any{
	keyChoices,
	provide("\t"),
	until("<inverse>"),
	expect("*print-ansi*"),
	until(keyBoxEnd),
}

func keyPick(key, want string) []any {
	return []any{provide(key), until("<inverse>"), expect(want), until(keyBoxEnd)}
}

func TestKeyCompleteArrows(t *testing.T) {
	withKeyBindings(t, "")
	keyEdTest(t, []any{
		keyFirstPick,
		keyPick("\x1b[C", "*print-array*"),
		keyPick("\x1b[D", "*print-ansi*"),
		keyPick("\x1b[B", "*print-case*"),
		keyPick("\x1b[A", "*print-ansi*"),
		keyPick("\x1b[A", "*print-readably*"),
	})
}

func TestKeyCompleteDownBeforePick(t *testing.T) {
	// With nothing highlighted down picks the first entry.
	withKeyBindings(t, "")
	keyEdTest(t, []any{keyChoices, keyPick("\x0e", "*print-ansi*")})
}

func TestKeyCompleteUpBeforePick(t *testing.T) {
	// With nothing highlighted up starts from the first entry and wraps.
	withKeyBindings(t, "")
	keyEdTest(t, []any{keyChoices, keyPick("\x10", "*print-readably*")})
}

func TestKeyCompleteBackBeforePick(t *testing.T) {
	withKeyBindings(t, "")
	keyEdTest(t, []any{keyChoices, keyPick("\x02", "*print-right-margin*")})
}

func TestKeyCompleteForwardBeforePick(t *testing.T) {
	withKeyBindings(t, "")
	keyEdTest(t, []any{keyChoices, keyPick("\x06", "*print-ansi*")})
}

func TestKeyCompleteReturnAccepts(t *testing.T) {
	withKeyBindings(t, "")
	keyEdTest(t, []any{
		keyFirstPick,
		keyPick("\x06", "*print-array*"),
		provide("\r"),
		until("<clear-down 3>"),
		until("/array\\*/"),
	})
}

func TestKeyCompleteNewlineAccepts(t *testing.T) {
	withKeyBindings(t, "")
	keyEdTest(t, []any{
		keyFirstPick,
		provide("\n"),
		until("<clear-down 3>"),
		until("/ansi\\*/"),
	})
}

func TestKeyCompleteReturnWithoutPick(t *testing.T) {
	// RET with nothing highlighted closes the list without inserting.
	withKeyBindings(t, "")
	keyEdTest(t, []any{
		keyChoices,
		provide("\r"),
		untilWithout{target: "<clear-down 3>", forbid: "/ansi/"},
		provide("x"),
		untilWithout{target: "x", forbid: "/ansi/"},
	})
}

func TestKeyCompleteReturnAcceptsMidLine(t *testing.T) {
	// Accepting with text after the cursor inserts before it.
	withKeyBindings(t, "")
	keyEdTest(t, []any{
		startSteps,
		provide(" x"),
		until("x"),
		provide("\x01"),
		until("<set-cursor 2:3>"),
		provide("*pri"),
		until("i x"),
		provide("\t"),
		until("/nt-/"),
		provide("\t"),
		until(keyBoxEnd),
		provide("\t"),
		until("<inverse>"),
		expect("*print-ansi*"),
		until(keyBoxEnd),
		provide("\r"),
		until("/ansi\\* x/"),
	})
}

func TestKeyCompleteEscCancels(t *testing.T) {
	withKeyBindings(t, "")
	keyEdTest(t, []any{
		keyFirstPick,
		provide("\x1b"),
		provide("x"),
		untilWithout{target: "x", forbid: "/ansi|undefined/"},
	})
}

func TestKeyCompleteOtherKeyDismisses(t *testing.T) {
	// Any other key closes the list and is then handled normally.
	withKeyBindings(t, "")
	keyEdTest(t, []any{
		keyFirstPick,
		provide("\x01"),
		untilWithout{target: "<set-cursor 2:3>", forbid: "/undefined/"},
	})
}

func TestKeyCompleteOtherCharDismisses(t *testing.T) {
	withKeyBindings(t, "")
	keyEdTest(t, []any{
		keyFirstPick,
		provide("z"),
		untilWithout{target: "z", forbid: "/undefined/"},
	})
}

// User keys in the completion list (addendum A item 6).

func TestKeyCompleteUserBackChar(t *testing.T) {
	withKeyBindings(t, `(repl-bind-key "C-l" 'back-char)`)
	keyEdTest(t, []any{
		keyFirstPick,
		keyPick("\x06", "*print-array*"),
		keyPick("\x0c", "*print-ansi*"),
	})
}

func TestKeyCompleteUserPreviousLine(t *testing.T) {
	withKeyBindings(t, `(repl-bind-key "C-l" 'previous-line)`)
	keyEdTest(t, []any{
		keyFirstPick,
		keyPick("\x0e", "*print-case*"),
		keyPick("\x0c", "*print-ansi*"),
	})
}

func TestKeyCompleteUserTab(t *testing.T) {
	withKeyBindings(t, `(repl-bind-key "C-l" 'tab)`)
	keyEdTest(t, []any{
		keyFirstPick,
		keyPick("\x0c", "*print-array*"),
	})
}

func TestKeyCompleteUserArrowRebound(t *testing.T) {
	// The right arrow rebound to back-char moves back in the list.
	withKeyBindings(t, `(repl-bind-key "M-[C" 'back-char)`)
	keyEdTest(t, []any{
		keyFirstPick,
		keyPick("\x06", "*print-array*"),
		keyPick("\x1b[C", "*print-ansi*"),
	})
}

func TestKeyCompleteUserEnter(t *testing.T) {
	withKeyBindings(t, `(repl-bind-key "C-l" 'enter)`)
	keyEdTest(t, []any{
		keyFirstPick,
		provide("\x0c"),
		until("<clear-down 3>"),
		until("/ansi\\*/"),
	})
}

func TestKeyCompleteUserNewline(t *testing.T) {
	withKeyBindings(t, `(repl-bind-key "C-l" 'newline)`)
	keyEdTest(t, []any{
		keyFirstPick,
		provide("\x0c"),
		until("<clear-down 3>"),
		until("/ansi\\*/"),
	})
}

func TestKeyCompleteUserUnrelatedDismisses(t *testing.T) {
	withKeyBindings(t, `(repl-bind-key "C-l" 'line-begin)`)
	keyEdTest(t, []any{
		keyFirstPick,
		provide("\x0c"),
		untilWithout{target: "<set-cursor 2:3>", forbid: "/undefined/"},
	})
}

func TestKeyCompleteDisabledDownDismisses(t *testing.T) {
	withKeyBindings(t, `(repl-bind-key "C-n" nil)`)
	keyEdTest(t, []any{
		keyFirstPick,
		provide("\x0e"),
		untilWithout{target: "/key C-n is undefined/", forbid: "*print-case*"},
	})
}

// With a single column (narrow terminal) moving up from no pick must wrap to
// the last entry. The up branch computes the last row from the entry count;
// with one column that row is past the end and the index is clamped.
func TestKeyCompleteSingleColumnUpWraps(t *testing.T) {
	withKeyBindings(t, "")
	keyEdTestSize(t, 40, 40, []any{keyChoices, keyPick("\x10", "*print-right-margin*")})
}

func TestKeyCompleteSingleColumnUpWrapsAccept(t *testing.T) {
	withKeyBindings(t, "")
	keyEdTestSize(t, 40, 40, []any{
		keyChoices,
		provide("\x10"),
		provide("\r"),
		until("<clear-down 3>"),
		until("*print-right-margin*"),
	})
}

func TestKeyCompleteTabWithoutWord(t *testing.T) {
	// TAB with no word before the cursor just keeps the cursor in place.
	withKeyBindings(t, "")
	keyEdTest(t, []any{
		startSteps,
		provide("( "),
		until(" "),
		provide("\t"),
		untilWithout{target: "<set-cursor 2:5>", forbid: "/undefined|┌/"},
	})
}

// TAB arriving in the same read as other bytes while the completion list is
// shown is still exempt from dismissing it (the check looks at the first
// byte, as before), with no user bindings.
func TestKeyCompleteTabWithMoreBytesKeepsList(t *testing.T) {
	withKeyBindings(t, "")
	keyEdTest(t, []any{
		keyChoices,
		provide("\t\x06"),
		untilWithout{target: keyBoxEnd, forbid: "/<clear-down/"},
	})
}

func TestKeyCompleteLoneEscClosesList(t *testing.T) {
	// A lone ESC (its own read) while the list is shown closes the list; the
	// next key is an ordinary key, not the second byte of an M- sequence.
	withKeyBindings(t, "")
	keyEdTest(t, []any{
		keyFirstPick,
		provide("\x1b"),
		until("/<clear-down/"),
		provide("f"),
		untilWithout{target: "f", forbid: "/undefined|<inverse>/"},
		provide("\x01"),
		untilWithout{target: "<set-cursor 2:3>", forbid: "/undefined/"},
	})
}

func TestKeyCompleteLoneEscBeforePick(t *testing.T) {
	withKeyBindings(t, "")
	keyEdTest(t, []any{
		keyChoices,
		provide("\x1b"),
		until("/<clear-down/"),
		provide("f"),
		untilWithout{target: "f", forbid: "/undefined|<inverse>/"},
	})
}
