// Copyright (c) 2026, Peter Ohler, All rights reserved.

package repl

import (
	"os"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/repl"
)

const keyStashFile = "config/keystash.lisp"

// keyStashSetup points the REPL stash at a fresh config/keystash.lisp holding
// "a", "b" and "c" (Run loads the stash named by *default-stash-name* from
// *stash-load-path*) and restores the settings afterwards.
func keyStashSetup(t *testing.T, setup string) {
	t.Helper()
	withKeyBindings(t, setup)
	tt.Nil(t, os.MkdirAll("config", 0755))
	tt.Nil(t, os.WriteFile(keyStashFile, []byte("\"a\"\n\"b\"\n\"c\"\n"), 0666))
	scope := repl.GetScope()
	origPath := scope.Get(slip.Symbol("*stash-load-path*"))
	origName := scope.Get(slip.Symbol("*default-stash-name*"))
	scope.Set(slip.Symbol("*stash-load-path*"), slip.List{slip.String("config")})
	scope.Set(slip.Symbol("*default-stash-name*"), slip.String("keystash.lisp"))
	t.Cleanup(func() {
		scope.Set(slip.Symbol("*stash-load-path*"), origPath)
		scope.Set(slip.Symbol("*default-stash-name*"), origName)
		_ = os.RemoveAll(keyStashFile)
	})
}

func TestKeyEdStashBackForward(t *testing.T) {
	keyStashSetup(t, "")
	keyEdTest(t, []any{
		startSteps,
		provide("\x1bp"),
		until("\"c\""),
		provide("\x1bp"),
		until("\"b\""),
		provide("\x1bp"),
		until("\"a\""),
		comment("at the oldest entry M-p does nothing, M-n goes forward"),
		provide("\x1bp"),
		provide("\x1bn"),
		untilWithout{target: "\"b\"", forbid: "/undefined/"},
		provide("\x1bn"),
		until("\"c\""),
		comment("past the newest entry the form is blank"),
		provide("\x1bn"),
		until("▶ "),
	})
}

func TestKeyEdStashForwardFromStart(t *testing.T) {
	keyStashSetup(t, "")
	keyEdTest(t, []any{
		startSteps,
		provide("\x1bn"),
		until("\"a\""),
		provide("\x1bn"),
		until("\"b\""),
	})
}

func TestKeyEdStashLeaveWithOtherKey(t *testing.T) {
	// A key that is not a stash key keeps the form and is handled normally.
	keyStashSetup(t, "")
	keyEdTest(t, []any{
		startSteps,
		provide("\x1bp"),
		until("\"c\""),
		until("<set-cursor 2:6>"),
		provide("\x01"),
		untilWithout{target: "<set-cursor 2:3>", forbid: "/undefined/"},
		comment("stash browsing is over so M-n starts from the first entry"),
		provide("\x1bn"),
		until("\"a\""),
	})
}

func TestKeyEdStashAdd(t *testing.T) {
	keyStashSetup(t, "")
	keyEdTest(t, []any{
		startSteps,
		provide("\"z\""),
		until("\""),
		until("z"),
		provide("\x1bs"),
		provide("\x15"),
		until("▶ "),
		provide("\x1bp"),
		until("\"z\""),
		provide("\x1bp"),
		until("\"c\""),
	})
}

func TestKeyEdStashSearchBack(t *testing.T) {
	keyStashSetup(t, "")
	keyEdTest(t, []any{
		startSteps,
		provide("\x1b,"),
		until("/search stash backwards: /"),
		provide("a"),
		until("\"a\""),
		until("/search stash backwards: a/"),
		provide("\x7f"),
		until("/search stash backwards: /"),
		comment("a control key ends the search"),
		provide("\x05"),
		untilWithout{target: "/<set-cursor/", forbid: "/undefined/"},
	})
}

func TestKeyEdStashSearchForward(t *testing.T) {
	keyStashSetup(t, "")
	keyEdTest(t, []any{
		startSteps,
		provide("\x1b."),
		until("/search stash forwards: /"),
		provide("b"),
		until("\"b\""),
		until("/search stash forwards: b/"),
		comment("M-. again searches on"),
		provide("\x1b."),
		until("/search stash forwards: b/"),
		provide("\x1b,"),
		until("/search stash backwards: b/"),
	})
}

// User rebound stash keys, recognized by the stash overrides through the
// user layer.

func TestKeyEdUserStashBack(t *testing.T) {
	keyStashSetup(t, `(repl-bind-key "C-l" 'stash-back)`)
	keyEdTest(t, []any{
		startSteps,
		provide("\x0c"),
		until("\"c\""),
		provide("\x0c"),
		until("\"b\""),
		provide("\x0c"),
		until("\"a\""),
	})
}

func TestKeyEdUserStashForward(t *testing.T) {
	keyStashSetup(t, `(repl-bind-key "C-l" 'stash-forward)`)
	keyEdTest(t, []any{
		startSteps,
		provide("\x0c"),
		until("\"a\""),
		provide("\x0c"),
		until("\"b\""),
	})
}

func TestKeyEdUserStashAdd(t *testing.T) {
	keyStashSetup(t, `(repl-bind-key "C-l" 'stash-add)`)
	keyEdTest(t, []any{
		startSteps,
		provide("\"z\""),
		until("z"),
		provide("\x0c"),
		provide("\x15"),
		until("▶ "),
		provide("\x1bp"),
		until("\"z\""),
	})
}

func TestKeyEdUserSearchStashBack(t *testing.T) {
	keyStashSetup(t, `(repl-bind-key "C-l" 'search-stash-back)`)
	keyEdTest(t, []any{
		startSteps,
		provide("\x0c"),
		until("/search stash backwards: /"),
		provide("a"),
		until("\"a\""),
		comment("C-l again continues the search"),
		provide("\x0c"),
		untilWithout{target: "/search stash backwards: a/", forbid: "/undefined/"},
	})
}

func TestKeyEdUserSearchStashForward(t *testing.T) {
	keyStashSetup(t, `(repl-bind-key "C-l" 'search-stash-forward)`)
	keyEdTest(t, []any{
		startSteps,
		provide("\x0c"),
		until("/search stash forwards: /"),
		provide("c"),
		until("\"c\""),
		provide("\x0c"),
		untilWithout{target: "/search stash forwards: c/", forbid: "/undefined/"},
	})
}

func TestKeyEdDisabledKeyEndsStash(t *testing.T) {
	// M-p disabled is not a stash key while browsing the stash.
	keyStashSetup(t, `(progn (repl-bind-key "C-l" 'stash-back) (repl-bind-key "M-p" nil))`)
	keyEdTest(t, []any{
		startSteps,
		provide("\x0c"),
		until("\"c\""),
		provide("\x1bp"),
		untilWithout{target: "/key M-p is undefined/", forbid: "\"b\""},
	})
}

func TestKeyEdDisabledKeyEndsStashSearch(t *testing.T) {
	keyStashSetup(t, `(progn (repl-bind-key "C-l" 'search-stash-back) (repl-bind-key "M-," nil))`)
	keyEdTest(t, []any{
		startSteps,
		provide("\x0c"),
		until("/search stash backwards: /"),
		provide("\x1b,"),
		until("/key M-, is undefined/"),
	})
}

func TestKeyEdUnrelatedUserKeyEndsStash(t *testing.T) {
	// A user key bound to a non-stash action ends browsing and runs.
	keyStashSetup(t, `(repl-bind-key "C-l" 'line-begin)`)
	keyEdTest(t, []any{
		startSteps,
		provide("\x1bp"),
		until("\"c\""),
		until("<set-cursor 2:6>"),
		provide("\x0c"),
		untilWithout{target: "<set-cursor 2:3>", forbid: "\"b\""},
	})
}

func TestKeyEdUnrelatedUserKeyEndsStashSearch(t *testing.T) {
	keyStashSetup(t, `(repl-bind-key "C-l" 'line-begin)`)
	keyEdTest(t, []any{
		startSteps,
		provide("\x1b,"),
		until("/search stash backwards: /"),
		provide("a"),
		until("\"a\""),
		until("/search stash backwards: a/"),
		provide("\x0c"),
		untilWithout{target: "<set-cursor 2:3>", forbid: "/search stash/"},
	})
}
