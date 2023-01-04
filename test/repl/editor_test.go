// Copyright (c) 2022, Peter Ohler, All rights reserved.

package repl

import (
	"fmt"
	"regexp"
	"strings"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/repl"
)

type expect string
type provide string
type until string
type comment string

var promptSeq = []any{
	expect("/<set-cursor [0-9]+:1>/"),
	expect("/<clear-line [0-9]+>/"),
	expect("<bold>"),
	expect("<bright-blue>"),
	expect("▶ "),
	expect("<normal>"),
}

var startSteps = []any{
	expect("Entering the SLIP REPL editor. Type ctrl-h for help and key bindings.\n"),
	promptSeq,
}

func skipLines(n int) []any {
	seq := make([]any, n)
	for n--; 0 <= n; n-- {
		seq[n] = expect("/.*/")
	}
	return seq
}

func match(ex, s string) bool {
	if ex == s {
		return true
	}
	if 2 < len(ex) && ex[0] == '/' && ex[len(ex)-1] == '/' {
		rx, err := regexp.Compile(ex[1 : len(ex)-1])
		if err == nil && rx.MatchString(s) {
			return true
		}
	}
	return false
}

func runScript(t *testing.T, tm *repl.Termock, script []any) {
	for _, x := range script {
		switch tx := x.(type) {
		case expect:
			out := tm.Output()
			if testing.Verbose() {
				fmt.Printf(">>> %q\n", out)
			}
			if !match(string(tx), out) {
				tm.Input("\x03") // C-c to exit
			}
			tt.Equal(t, string(tx), out)
		case until:
			for {
				out := tm.Output()
				if testing.Verbose() {
					fmt.Printf(">>> %q\n", out)
				}
				if match(string(tx), out) {
					break
				}
			}
		case provide:
			if testing.Verbose() {
				fmt.Printf("<<< %q\n", tx)
			}
			tm.Input(string(tx))
		case comment:
			if testing.Verbose() {
				fmt.Printf("### %s\n", tx)
			}
		case []any:
			runScript(t, tm, tx)
		}
	}
}

func edTest(t *testing.T, script []any) {
	tm := repl.NewTermock(40, 80)
	scope := repl.GetScope()
	scope.Set(slip.Symbol("x"), slip.Fixnum(3))
	scope.Set(slip.Symbol("*standard-output*"), tm)
	scope.Set(slip.Symbol("*standard-input*"), tm)
	repl.SetConfigDir("config")
	scope.Set(slip.Symbol("*repl-editor*"), slip.True)

	go runScript(t, tm, script)

	repl.Run()
	for {
		out := tm.Output()
		if match("/Bye/", out) {
			break
		}
	}
}

func TestEditorEmpty(t *testing.T) {
	edTest(t, []any{startSteps, provide("\x03")})
}

func TestEditorEval(t *testing.T) {
	edTest(t, []any{
		startSteps,
		provide("x"),
		expect("x"),
		provide("\r"),
		expect("<set-cursor 9999:9999>"),
		expect("<set-cursor 3:1>"),
		expect("3\n"),
		promptSeq,
		provide("\x03"),
	})
}

func TestEditorHelp(t *testing.T) {
	edTest(t, []any{
		startSteps,
		provide("\x08"),
		expect("<set-cursor 9999:9999>"),
		expect("<set-cursor 4:4>"),
		expect("<bold>"),
		expect("SLIP REPL Editor"),
		expect("<normal>"),
		// expect some help description followed by key bindings.
		expect("/This editor.*/"),
		skipLines(11),
		expect("/bindings are:.*/"),
		expect("<bold>"),
		expect("C-a"),
		expect("<normal>"),
		expect("   move to line start              \n   "),
		until("┌────────────────────────────────────────────────────────────────────┒"),
		until("┕━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛"),
		expect("<set-cursor 2:3>"),
		provide("\x01"),
		expect("<set-cursor 3:1>"),
		expect("<clear-down 3>"),
		until("<set-cursor 2:3>"),
		provide("\x03"),
	})
}

func TestEditorDescribeScroll(t *testing.T) {
	edTest(t, []any{
		startSteps,
		provide(strings.Repeat("\n", 20)),
		until("<clear-to-start 22:3>"),
		expect("<set-cursor 22:3>"),
		provide("(car"),
		until("("),
		expect("c"),
		expect("a"),
		expect("r"),
		provide("\x1b/"),
		until("<scroll-up 1>"),
		expect("<set-cursor 23:4>"),
		expect("<bold>"),
		expect("car"),
		expect("<normal>"),
		until("┕━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛"),
		expect("<set-cursor 21:7>"),
		provide("\x03"),
	})
}

func TestEditorUnknownControlKey(t *testing.T) {
	testEditorUnknownKey(t, "\x0c")
}

func TestEditorUnknownMetaKey(t *testing.T) {
	testEditorUnknownKey(t, "\x1b\x10")
}

func TestEditorUnknownMetaBracketKey(t *testing.T) {
	testEditorUnknownKey(t, "\x1b\x5bZ")
}

func TestEditorUnknownUnicodeKey(t *testing.T) {
	testEditorUnknownKey(t, "\xeeA")
}

func TestEditorUnknownMetaMetaDelKey(t *testing.T) {
	testEditorUnknownKey(t, "\x1b\x1b\x7f")
}

func TestEditorBadUnicode(t *testing.T) {
	edTest(t, []any{
		startSteps,
		provide("\xee\xee\xee\xee\xee\xee"),
		until("<inverse>"),
		expect("  "),
		expect("/invalid UTF-8 sequence: \\[\\]byte{0xee, 0xee, 0xee, 0xee, 0xee, 0xee}.*/"),
		expect("<normal>"),
		provide("\x03"),
	})
}

func testEditorUnknownKey(t *testing.T, key string) {
	edTest(t, []any{
		startSteps,
		provide(key),
		until("<inverse>"),
		expect("  "),
		expect("/key .+ is undefined. sequence: \\[\\]byte{.+} +/"),
		expect("<normal>"),
		provide("\x03"),
	})
}

func TestEditorComplete(t *testing.T) {
	edTest(t, []any{
		startSteps,
		provide("*pri"),
		until("*"),
		expect("p"),
		expect("r"),
		expect("i"),
		comment("press <tab>"),
		provide("\t"),
		until("nt-"), // complete as much as common

		comment("press <tab> again"),
		provide("\t"), // show choices
		until("┕━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛"),
		expect("<set-cursor 2:10>"),

		comment("press <tab> to pick first entry"),
		provide("\t"), // highlight first choice
		until("<inverse>"),
		expect("*print-ansi*"),
		expect("<normal>"),
		until("┕━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛"),
		expect("<set-cursor 2:10>"),

		comment("press C-f"),
		provide("\x06"), // highlight next choice
		until("<inverse>"),
		expect("*print-array*"),
		expect("<normal>"),
		until("┕━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛"),
		expect("<set-cursor 2:10>"),

		comment("press C-n"),
		provide("\x0e"),
		until("<inverse>"),
		expect("*print-circle*"),
		expect("<normal>"),
		until("┕━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛"),
		expect("<set-cursor 2:10>"),

		comment("press C-b"),
		provide("\x02"),
		until("<inverse>"),
		expect("*print-case*"),
		expect("<normal>"),
		until("┕━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛"),
		expect("<set-cursor 2:10>"),

		comment("press C-p"),
		provide("\x10"),
		until("<inverse>"),
		expect("*print-ansi*"),
		expect("<normal>"),
		until("┕━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛"),
		expect("<set-cursor 2:10>"),

		provide("\r"), // make a choice
		until("<clear-down 3>"),
		until("ansi*"),

		provide("\x03"),
	})
}

func TestEditorString(t *testing.T) {
	testEditorString(t, `"abc"`, `"abc"`)
}

func TestEditorUnicodeString(t *testing.T) {
	testEditorString(t, `ピーター`, `ピーター`)
}

func testEditorString(t *testing.T, seq string, expect string) {
	want := []any{startSteps, provide(seq)}
	for _, r := range []rune(expect) {
		want = append(want, until(string([]rune{r})))
	}
	want = append(want, provide("\x03"))
	edTest(t, want)
}
