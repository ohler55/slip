// Copyright (c) 2026, Peter Ohler, All rights reserved.

package repl

import (
	"os"
	"strings"
	"testing"
	"time"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/repl"
)

// cursorTest runs the editor with a Termock. The setup function is called
// before the editor starts and the keys function is called with a step
// function that provides input and waits for a pattern in the output. If the
// pattern is not seen before the step times out, C-c is sent so the test
// fails instead of hanging. All output is returned.
func cursorTest(t *testing.T, setup func(tm *repl.Termock), keys func(step func(input, want string) bool)) string {
	err := os.RemoveAll("config/history")
	tt.Nil(t, err)
	err = os.RemoveAll("config/config.lisp")
	tt.Nil(t, err)
	tm := repl.NewTermock(40, 80)
	defer repl.SetSizer(nil)
	repl.SetSizer(tm)
	scope := repl.GetScope()
	scope.Set(slip.Symbol("*standard-output*"), tm)
	scope.Set(slip.Symbol("*standard-input*"), tm)
	repl.SetConfigDir("config")
	setup(tm)
	scope.Set(slip.Symbol("*repl-editor*"), slip.True)

	outChan := make(chan string, 1000)
	go func() {
		for {
			s := tm.Output()
			outChan <- s
			if strings.Contains(s, "Bye") {
				return
			}
		}
	}()
	exited := make(chan bool)
	go func() {
		repl.Run()
		close(exited)
	}()
	var all strings.Builder
	step := func(input, want string) bool {
		if 0 < len(input) {
			tm.Input(input)
		}
		deadline := time.After(3 * time.Second)
		for {
			select {
			case s := <-outChan:
				all.WriteString(s)
				if strings.Contains(all.String(), want) {
					return true
				}
			case <-deadline:
				t.Errorf("timed out waiting for %q", want)
				return false
			}
		}
	}
	step("", "Entering the SLIP REPL editor")
	keys(step)
	for {
		tm.Input("\x03")
		select {
		case <-exited:
			for 0 < len(outChan) {
				all.WriteString(<-outChan)
			}
			return all.String()
		case s := <-outChan:
			all.WriteString(s)
		case <-time.After(time.Second):
		}
	}
}

func TestEditorKeyBeforeCursorReport(t *testing.T) {
	// A C-h typed while the terminal is still answering a cursor position
	// query must show help, not be swallowed as the report, and the report
	// must not be read as an undefined key.
	out := cursorTest(t,
		func(tm *repl.Termock) { tm.SendBeforeCursorReport("\x08", false) },
		func(step func(string, string) bool) {
			step("", "SLIP REPL Editor")
		})
	tt.Equal(t, false, strings.Contains(out, "undefined"))
}

func TestEditorKeyWithCursorReportInOneRead(t *testing.T) {
	// The key and the report arrive in a single read.
	out := cursorTest(t,
		func(tm *repl.Termock) { tm.SendBeforeCursorReport("\x08", true) },
		func(step func(string, string) bool) {
			step("", "SLIP REPL Editor")
		})
	tt.Equal(t, false, strings.Contains(out, "undefined"))
}

func TestEditorLateCursorReport(t *testing.T) {
	// The report to the first query arrives after getCursor gave up. It must
	// still be removed from the input and the next query must get its own
	// report.
	var term *repl.Termock
	out := cursorTest(t,
		func(tm *repl.Termock) {
			term = tm
			tm.HoldCursorReports(1)
		},
		func(step func(string, string) bool) {
			// Release only after the prompt is shown so the queries have
			// already timed out.
			step("", "▶")
			term.ReleaseCursorReports()
			step("(+ 1000 234)\r", "1234")
			step("(+ 4000 321)\r", "4321")
		})
	tt.Equal(t, false, strings.Contains(out, "undefined"))
}

func TestEditorLateCursorReportsInOneRead(t *testing.T) {
	// Two late reports arrive together after both queries timed out. One is
	// kept for the next query to discard and the other is dropped.
	var term *repl.Termock
	out := cursorTest(t,
		func(tm *repl.Termock) {
			term = tm
			tm.HoldCursorReports(2)
		},
		func(step func(string, string) bool) {
			// Release only after the prompt is shown so the queries have
			// already timed out.
			step("", "▶")
			term.ReleaseCursorReports()
			step("(+ 1000 234)\r", "1234")
			step("(+ 4000 321)\r", "4321")
		})
	tt.Equal(t, false, strings.Contains(out, "undefined"))
}

func TestEditorCursorReportPatternWithoutQuery(t *testing.T) {
	// ESC [ 1 ; 2 R with no query outstanding is a key (shift-F3 on some
	// terminals), not a cursor position report.
	cursorTest(t,
		func(tm *repl.Termock) {},
		func(step func(string, string) bool) {
			step("\x1b[1;2R", "undefined")
		})
}
