// Copyright (c) 2026, Peter Ohler, All rights reserved.

package repl

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip/pkg/repl"
)

// withClipboard puts a pbpaste on the PATH that prints text so C-y can be
// tested without the system clipboard.
func withClipboard(t *testing.T, text string) {
	dir := t.TempDir()
	script := "#!/bin/sh\nprintf '%s' \"$PASTE_TEXT\"\n"
	err := os.WriteFile(filepath.Join(dir, "pbpaste"), []byte(script), 0755)
	tt.Nil(t, err)
	t.Setenv("PATH", dir+string(os.PathListSeparator)+os.Getenv("PATH"))
	t.Setenv("PASTE_TEXT", text)
}

func TestEditorPasteLongLine(t *testing.T) {
	// A C-y paste longer than the 32 byte key buffer.
	withClipboard(t, `(list "a long enough string to go past thirty-two bytes")`)
	out := cursorTest(t,
		func(tm *repl.Termock) {},
		func(step func(string, string) bool) {
			step("\x19", "thirty-two bytes\")")
			step("\r", `("a long enough string to go past thirty-two bytes")`)
		})
	tt.Equal(t, false, strings.Contains(out, "runtime error"))
}

func TestEditorPasteMultipleLines(t *testing.T) {
	// A C-y paste of a form over several lines.
	withClipboard(t, "(list 11\n      22 \"a string long enough to span reads\")")
	out := cursorTest(t,
		func(tm *repl.Termock) {},
		func(step func(string, string) bool) {
			step("\x19", "span reads\")")
			step("\r", `(11 22 "a string long enough to span reads")`)
		})
	tt.Equal(t, false, strings.Contains(out, "runtime error"))
}

func TestEditorPasteMultipleForms(t *testing.T) {
	// A C-y paste of several forms, one longer than the key buffer, is
	// evaluated form by form on enter.
	withClipboard(t, "(+ 1000 234)\n(list \"a string long enough to span reads\")\n(+ 4000 321)")
	out := cursorTest(t,
		func(tm *repl.Termock) {},
		func(step func(string, string) bool) {
			step("\x19", "(+ 4000 321)")
			step("\r", "4321")
		})
	tt.Equal(t, true, strings.Contains(out, "1234"))
	tt.Equal(t, true, strings.Contains(out, `("a string long enough to span reads")`))
	tt.Equal(t, false, strings.Contains(out, "runtime error"))
}

func TestEditorTerminalPasteMultipleForms(t *testing.T) {
	// A terminal paste arrives as one burst of input with a carriage return
	// at each line end, read 32 bytes at a time. Every form is evaluated,
	// including the one that spans two lines.
	out := cursorTest(t,
		func(tm *repl.Termock) {},
		func(step func(string, string) bool) {
			step("(+ 1000 234)\r(list 11\r      22 \"a string long enough to span reads\")\r(+ 4000 321)\r", "4321")
		})
	tt.Equal(t, true, strings.Contains(out, "1234"))
	tt.Equal(t, true, strings.Contains(out, `(11 22 "a string long enough to span reads")`))
	tt.Equal(t, false, strings.Contains(out, "undefined"))
}

func TestEditorExitWithFullKeyQueue(t *testing.T) {
	// Keys typed while a slow form evaluates fill the key queue. Exiting
	// with the queue full must not hang.
	var term *repl.Termock
	cursorTest(t,
		func(tm *repl.Termock) { term = tm },
		func(step func(string, string) bool) {
			term.Input("(sleep 1)\r")
			go func() {
				term.Input("\x03")
				for range 110 {
					term.Input("a")
				}
			}()
			step("", "Bye")
		})
}
