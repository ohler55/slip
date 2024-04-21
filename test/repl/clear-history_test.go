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

func TestClearHistoryAll(t *testing.T) {
	repl.TheHistory.SetLimit(100)
	filename := "./config/history"
	err := os.WriteFile(filename, []byte("one\n(+ 1\t   2\t   3)\n\n(* 2\n   four)\n"), 0666)
	tt.Nil(t, err)
	repl.TheHistory.LoadExpanded(filename)
	(&sliptest.Function{
		Source: `(clear-history)`,
		Expect: "nil",
	}).Test(t)
	tt.Equal(t, 0, repl.TheHistory.Size())
}

func TestClearHistoryStartEnd(t *testing.T) {
	repl.TheHistory.SetLimit(100)
	filename := "./config/history"
	err := os.WriteFile(filename, []byte("one\n(+ 1\t   2\t   3)\n\n(* 2\n   four)\n"), 0666)
	tt.Nil(t, err)
	repl.TheHistory.LoadExpanded(filename)
	(&sliptest.Function{
		Source: `(clear-history :start 1 :end -1)`,
		Expect: "nil",
	}).Test(t)
	tt.Equal(t, 1, repl.TheHistory.Size())
}

func TestClearHistoryBadStart(t *testing.T) {
	(&sliptest.Function{
		Source:    `(clear-history :start t)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}

func TestClearHistoryBadEnd(t *testing.T) {
	(&sliptest.Function{
		Source:    `(clear-history :end t)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}
