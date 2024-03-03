// Copyright (c) 2024, Peter Ohler, All rights reserved.

package repl

import (
	"fmt"
	"os"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestUseStashPath(t *testing.T) {
	filename := "config/stash"
	err := os.WriteFile(filename, []byte("one\n(+ 1\t   2\t   3)\n\n(* 2\n   four)\n"), 0666)
	tt.Nil(t, err)

	filename = "config/dash.lisp"
	err = os.WriteFile(filename, []byte("dash\n(* 2 3)\n"), 0666)
	tt.Nil(t, err)

	(&sliptest.Function{
		Source: fmt.Sprintf(`(progn (use-stash %q) (show-stash nil :tight t))`, filename),
		Expect: `"dash
(* 2 3)
"`,
	}).Test(t)
}

func TestUseStashPathCreate(t *testing.T) {
	filename := "config/stash"
	err := os.WriteFile(filename, []byte("one\n(+ 1\t   2\t   3)\n\n(* 2\n   four)\n"), 0666)
	tt.Nil(t, err)

	filename = "config/dash.lisp"
	_ = os.RemoveAll(filename)

	(&sliptest.Function{
		Source: fmt.Sprintf(`(progn (use-stash %q) (show-stash nil :tight t))`, filename),
		Expect: `""`,
	}).Test(t)
	// veridy the file exists
	_, err = os.Stat(filename)
	tt.Nil(t, err)
}

func TestUseStashName(t *testing.T) {
	filename := "config/stash"
	err := os.WriteFile(filename, []byte("one\n(+ 1\t   2\t   3)\n\n(* 2\n   four)\n"), 0666)
	tt.Nil(t, err)

	filename = "config/dash.lisp"
	err = os.WriteFile(filename, []byte("dash\n(* 2 3)\n"), 0666)
	tt.Nil(t, err)

	(&sliptest.Function{
		Source: `(let ((*stash-load-path* '("config")))
                  (use-stash 'dash)
                  (show-stash nil :tight t))`,
		Expect: `"dash
(* 2 3)
"`,
	}).Test(t)
}

func TestUseStashEmptyLoadPath(t *testing.T) {
	filename := "config/stash"
	err := os.WriteFile(filename, []byte("one\n(+ 1\t   2\t   3)\n\n(* 2\n   four)\n"), 0666)
	tt.Nil(t, err)

	filename = "dash.lisp"
	_ = os.RemoveAll(filename)
	defer func() { _ = os.RemoveAll(filename) }()

	(&sliptest.Function{
		Source: `(let ((*stash-load-path* '()))
                  (use-stash 'dash)
                  (show-stash nil :tight t))`,
		Expect: `""`,
	}).Test(t)
}

func TestUseStashBadName(t *testing.T) {
	(&sliptest.Function{
		Source:    `(use-stash "")`,
		PanicType: slip.Symbol("error"),
	}).Test(t)
}
