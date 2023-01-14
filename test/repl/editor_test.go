// Copyright (c) 2022, Peter Ohler, All rights reserved.

package repl

import (
	"fmt"
	"os"
	"regexp"
	"strings"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/repl"
)

const debugScript = true

type expect string
type provide string
type until string
type comment string

var promptSeq = []any{
	expect("/<set-cursor [0-9]+:1>/"),
	until("/<clear-line [0-9]+>/"),
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
			if testing.Verbose() && debugScript {
				fmt.Printf("??? waiting for %q\n", tx)
			}
			for {
				out := tm.Output()
				if testing.Verbose() {
					fmt.Printf(">>> %q\n", out)
				}
				if match(string(tx), out) {
					if testing.Verbose() && debugScript {
						fmt.Printf("--- matched %q\n", out)
					}
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
	err := os.RemoveAll("config/history")
	tt.Nil(t, err)
	err = os.RemoveAll("config/config.lisp")
	tt.Nil(t, err)
	tm := repl.NewTermock(40, 80)
	defer repl.SetSizer(nil)
	repl.SetSizer(tm)
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
		expect("/move to line start/"),
		until("/┌──────────────*────────────┒/"),
		until("/┕━━━━━━━━━━━━━━*━━━━━━━━━━━━┛/"),
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
		until("<clear-to-end 22:3>"),
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
		until("/┕━━━━━━━━━━━━━━*━━━━━━━━━━━━┛/"),
		expect("<set-cursor 21:7>"),
		provide("\x03"),
	})
}

func TestEditorDescribeUnknown(t *testing.T) {
	edTest(t, []any{
		startSteps,
		provide("( "),
		until("("),
		expect(" "),
		provide("\x02"),
		provide("\x1b/"),
		until("<inverse>"),
		until("/could not determine what to describe/"),
		until("<set-cursor 2:4>"),
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

func TestEditorTabComplete(t *testing.T) {
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
		until("/┕━━━━━━━━━━━━━━*━━━━━━━━━━━━┛/"),
		expect("<set-cursor 2:10>"),

		comment("press <tab> to pick first entry"),
		provide("\t"), // highlight first choice
		until("<inverse>"),
		expect("*print-ansi*"),
		expect("<normal>"),
		until("/┕━━━━━━━━━━━━━━*━━━━━━━━━━━━┛/"),
		expect("<set-cursor 2:10>"),

		comment("press C-f"),
		provide("\x06"), // highlight next choice
		until("<inverse>"),
		expect("*print-array*"),
		expect("<normal>"),
		until("/┕━━━━━━━━━━━━━━*━━━━━━━━━━━━┛/"),
		expect("<set-cursor 2:10>"),

		comment("press C-n"),
		provide("\x0e"),
		until("<inverse>"),
		expect("*print-circle*"),
		expect("<normal>"),
		until("/┕━━━━━━━━━━━━━━*━━━━━━━━━━━━┛/"),
		expect("<set-cursor 2:10>"),

		comment("press C-b"),
		provide("\x02"),
		until("<inverse>"),
		expect("*print-case*"),
		expect("<normal>"),
		until("/┕━━━━━━━━━━━━━━*━━━━━━━━━━━━┛/"),
		expect("<set-cursor 2:10>"),

		comment("press C-p"),
		provide("\x10"),
		until("<inverse>"),
		expect("*print-ansi*"),
		expect("<normal>"),
		until("/┕━━━━━━━━━━━━━━*━━━━━━━━━━━━┛/"),
		expect("<set-cursor 2:10>"),

		comment("press C-b at first choice"),
		provide("\x02"), // highlight next choice
		until("<inverse>"),
		expect("*print-right-margin*"),
		until("<set-cursor 2:10>"),

		comment("press C-f at last choice"),
		provide("\x06"), // highlight next choice
		until("<inverse>"),
		expect("*print-ansi*"),
		until("<set-cursor 2:10>"),

		comment("press C-p at top line"),
		provide("\x10"), // highlight next choice
		until("<inverse>"),
		expect("*print-readably*"),
		until("<set-cursor 2:10>"),

		comment("press C-n at last line"),
		provide("\x0e"), // highlight next choice
		until("<inverse>"),
		expect("*print-ansi*"),
		until("<set-cursor 2:10>"),

		provide("\r"), // make a choice
		until("<clear-down 3>"),
		until("ansi*"),

		provide("\x03"),
	})
}

func TestEditorHistory(t *testing.T) {
	edTest(t, []any{
		startSteps,
		// Create some history.
		provide("\"a\"\r"),
		until("▶ "),
		expect("<normal>"),

		provide("\"b\"\r"),
		until("▶ "),
		expect("<normal>"),

		provide("\"c\"\r"),
		until("▶ "),
		expect("<normal>"),

		provide("\x1bv"),
		until("\"c\""),
		until("/<set-cursor .*>/"),

		provide("\x1bv"),
		until("\"b\""),
		until("/<set-cursor .*>/"),

		provide("\x1bv"),
		until("\"a\""),
		until("/<set-cursor .*>/"),

		provide("\x1bv"), // nothing should happen

		provide("\x16"),
		until("\"b\""),
		until("/<set-cursor .*>/"),

		provide("\x16"),
		until("\"c\""),
		until("/<set-cursor .*>/"),

		provide("\x16"),
		until("▶ "), // back to a blank line
		expect("<normal>"),

		provide(" \x16"),
		until("\"a\""),
		until("/<set-cursor .*>/"),

		provide("\x13"),
		until("/search forwards:   /"),
		provide("b"),

		until("\"b\""),
		until("/search forwards: b/"),
		until("/<set-cursor .*>/"),

		provide("\x7f"),
		until("/search forwards:   /"),
		until("/<set-cursor .*>/"),

		provide("\x12"),
		until("/search backwards:   /"),
		until("/<set-cursor .*>/"),

		provide("b"),
		until("/search backwards: b/"),
		until("/<set-cursor .*>/"),

		provide("\x03"),
	})
}

func TestEditorString(t *testing.T) {
	testEditorString(t, `"abc"`, `"abc"`)
}

func TestEditorUnicodeString(t *testing.T) {
	testEditorString(t, `ピーター`, `ピーター`)
}

func TestEditorParenMatch(t *testing.T) {
	testEditorString(t, "(+ 1\n 2)",
		"(+ 1<set-cursor 3:3> 2)<set-cursor 2:3><bold>(<normal><set-cursor 3:6>")
}

func TestEditorLeftRight(t *testing.T) {
	testEditorSeq(t, "a\nb\x02\x02\x02\x02\x06\x06\x06\x06",
		"a", "<set-cursor 2:4>", "<set-cursor 3:3>", "b",
		"<set-cursor 3:3>", "<set-cursor 2:4>", "<set-cursor 2:3>", "<set-cursor 2:3>",
		"<set-cursor 2:4>", "<set-cursor 3:3>", "<set-cursor 3:4>", "<set-cursor 3:4>",
	)
}

func TestEditorUp(t *testing.T) {
	testEditorSeq(t, "a\nbc\x10\x10",
		"a", "<set-cursor 3:3>", "b", "c",
		"<set-cursor 2:4>", "<set-cursor 2:4>",
	)
}

func TestEditorDown(t *testing.T) {
	testEditorSeq(t, "ab\nc\x10\x06\x0e\x0e",
		"a", "b", "<set-cursor 3:3>", "c",
		"<set-cursor 2:4>", "<set-cursor 2:5>",
		"<set-cursor 3:4>", "<set-cursor 3:4>",
	)
}

func TestEditorNL(t *testing.T) {
	testEditorSeq(t, "ab\x02\n",
		"a", "b", "<set-cursor 2:4>",
		"<set-cursor 3:3>",
		"<clear-to-end 3:3>", "b", "<set-cursor 3:3>",
	)
}

func TestEditorForwardBackWord(t *testing.T) {
	testEditorSeq(t, "abc def\x1bb\x1bb\x1bf\x1bf",
		"a", "b", "c", " ", "d", "e", "f",
		"<set-cursor 2:7>", "<set-cursor 2:3>",
		"<set-cursor 2:6>", "<set-cursor 2:10>",
	)
}

func TestEditorLineStartEnd(t *testing.T) {
	testEditorSeq(t, "abc\x01\x05",
		"a", "b", "c",
		"<set-cursor 2:3>", "<set-cursor 2:6>",
	)
}

func TestEditorMatchOpenClose(t *testing.T) {
	testEditorSeq(t, "(())\x1b\x02\x1b\x06",
		"(", "(", ")", ")",
		"<set-cursor 2:3>",
		"<set-cursor 2:7>",
	)
}

func TestEditorDelWord(t *testing.T) {
	testEditorSeq(t, "abc def\x1b\x7f\x01\x1b\x64",
		"a", "b", "c", " ", "d", "e", "f",
		"<set-cursor 2:3>", "abc ",
		"<set-cursor 2:3>",
		"<clear-to-end 2:3>", " ",
		"<set-cursor 2:3>",
	)
}

func TestEditorDelChar(t *testing.T) {
	testEditorSeq(t, "abc\x7f\x01\x04",
		"a", "b", "c",
		"<set-cursor 2:5>", "<clear-to-end 2:5>", "<set-cursor 2:5>",
		"<set-cursor 2:3>",
		"<set-cursor 2:3>", "<clear-to-end 2:3>", "b",
		"<set-cursor 2:3>",
	)
}

func TestEditorDelBackwardLine(t *testing.T) {
	testEditorSeq(t, "ab\ncd\x02\x02\x7f\x05",
		"a", "b", "<set-cursor 3:3>", "c", "d",
		"<set-cursor 3:4>",
		"<set-cursor 3:3>",
		"<clear-line 3>", "<set-cursor 2:3>", "abcd", "<set-cursor 2:7>",
	)
}

func TestEditorDelForwardLine(t *testing.T) {
	testEditorSeq(t, "ab\ncd\x10\x04\x05",
		"a", "b", "<set-cursor 3:3>", "c", "d",
		"<set-cursor 2:5>",
		"<clear-line 3>", "<set-cursor 2:3>", "abcd", "<set-cursor 2:7>",
	)
}

func TestEditorDelToEnd(t *testing.T) {
	testEditorSeq(t, "abc\ndef\x10\x01\x06\x0b\x0b",
		"a", "b", "c", "<set-cursor 3:3>", "d", "e", "f",
		"<set-cursor 2:6>", "<set-cursor 2:4>",
		"<clear-to-end 2:4>", "<set-cursor 2:4>",
		"<clear-line 3>", "<clear-line 2>", "adef",
		"<set-cursor 2:4>",
	)
}

func TestEditorSwapChar(t *testing.T) {
	testEditorSeq(t, "abcd\x02\x02\x14",
		"a", "b", "c", "d",
		"<set-cursor 2:5>",
		"<set-cursor 2:4>", "cb", "<set-cursor 2:5>",
	)
}

func TestEditorCollapse(t *testing.T) {
	testEditorSeq(t, "ab  \x1b\x5c",
		"a", "b", " ", " ",
		"<set-cursor 2:5>", "<clear-to-end 2:5>",
		"<set-cursor 2:5>",
	)
	testEditorSeq(t, "  ab\x01\x1b\x5c",
		" ", " ", "a", "b",
		"<set-cursor 2:3>", "<clear-to-end 2:3>", "ab",
		"<set-cursor 2:3>",
	)
}

func TestEditorNlAfter(t *testing.T) {
	testEditorSeq(t, "abcd\x02\x02\x0f",
		"a", "b", "c", "d",
		"<set-cursor 2:5>",
		"<clear-to-end 2:5>",
		"<set-cursor 3:3>", "cd", "<set-cursor 2:5>",
	)
}

// TBD
func xTestEditorEnterUnicode(t *testing.T) {
	testEditorSeq(t, "\x1b\x5530d4\rx",
		"z",
		"<inverse>", "/unicode: /", "3", "0", "d", "4",
		"<clear-to-end 4>",
		// TBD
		"<set-cursor 2:4>",
	)
}

func testEditorSeq(t *testing.T, seq string, expect ...string) {
	want := []any{startSteps, provide(seq)}
	for _, x := range expect {
		want = append(want, until(x))
	}
	want = append(want, provide("\x03"))

	edTest(t, want)
}

func testEditorString(t *testing.T, seq string, expect string) {
	want := []any{startSteps, provide(seq)}
	ra := []rune(expect)
	for i := 0; i < len(ra); i++ {
		r := ra[i]
		if r == '<' {
			if end := strings.IndexByte(expect[i:], '>'); 0 < end {
				want = append(want, until(string(ra[i:i+end+1])))
				i += end
			} else {
				want = append(want, until(string([]rune{r})))
			}
		} else {
			want = append(want, until(string([]rune{r})))
		}
	}
	want = append(want, provide("\x03"))

	edTest(t, want)
}
