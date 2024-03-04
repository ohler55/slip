// Copyright (c) 2024, Peter Ohler, All rights reserved.

package repl

import (
	"os"
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/repl"
	"github.com/ohler55/slip/sliptest"
)

func TestClearStashAll(t *testing.T) {
	filename := "config/stash"
	err := os.WriteFile(filename, []byte("one\n(+ 1\t   2\t   3)\n\n(* 2\n   four)\n"), 0666)
	tt.Nil(t, err)
	repl.TheStash.LoadExpanded(filename)
	(&sliptest.Function{
		Source: `(clear-stash)`,
		Expect: "nil",
	}).Test(t)
	tt.Equal(t, 0, repl.TheStash.Size())
}

func TestClearStashStartEnd(t *testing.T) {
	filename := "config/stash"
	err := os.WriteFile(filename, []byte("one\n(+ 1\t   2\t   3)\n\n(* 2\n   four)\n"), 0666)
	tt.Nil(t, err)
	repl.TheStash.LoadExpanded(filename)
	(&sliptest.Function{
		Source: `(clear-stash :start 1 :end -1)`,
		Expect: "nil",
	}).Test(t)
	tt.Equal(t, 1, repl.TheStash.Size())
}

func TestClearStashBadStart(t *testing.T) {
	(&sliptest.Function{
		Source:    `(clear-stash :start t)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}

func TestClearStashBadEnd(t *testing.T) {
	(&sliptest.Function{
		Source:    `(clear-stash :end t)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}
