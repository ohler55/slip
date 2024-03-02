// Copyright (c) 2024, Peter Ohler, All rights reserved.

package repl

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/repl"
	"github.com/ohler55/slip/sliptest"
)

func TestNthHistoryOk(t *testing.T) {
	repl.TheHistory.Clear(0, -1)
	repl.TheHistory.Add(repl.NewForm([]byte("(+ 1 2)")))
	repl.TheHistory.Add(repl.NewForm([]byte("(* 2 3)")))
	repl.TheHistory.Add(repl.NewForm([]byte("(+ 3 4) (- 4 3)")))
	(&sliptest.Function{
		Source: `(nth-history 1)`,
		Expect: "(+ 1 2)",
	}).Test(t)
	(&sliptest.Function{
		Source: `(nth-history 2)`,
		Expect: "(* 2 3)",
	}).Test(t)
	(&sliptest.Function{
		Source: `(nth-history 3)`,
		Expect: "((+ 3 4) (- 4 3))",
	}).Test(t)
}

func TestNthHistoryBadIndex(t *testing.T) {
	(&sliptest.Function{
		Source:    `(nth-history t)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}
