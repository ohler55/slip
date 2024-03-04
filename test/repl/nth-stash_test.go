// Copyright (c) 2024, Peter Ohler, All rights reserved.

package repl

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/repl"
	"github.com/ohler55/slip/sliptest"
)

func TestNthStashOk(t *testing.T) {
	repl.TheStash.LoadExpanded("config/stash")
	repl.TheStash.Clear(0, -1)
	repl.TheStash.Add(repl.NewForm([]byte("(+ 1 2)")))
	repl.TheStash.Add(repl.NewForm([]byte("(* 2 3)")))
	repl.TheStash.Add(repl.NewForm([]byte("(+ 3 4) (- 4 3)")))
	(&sliptest.Function{
		Source: `(nth-stash 1)`,
		Expect: "(+ 1 2)",
	}).Test(t)
	(&sliptest.Function{
		Source: `(nth-stash 2)`,
		Expect: "(* 2 3)",
	}).Test(t)
	(&sliptest.Function{
		Source: `(nth-stash 3)`,
		Expect: "((+ 3 4) (- 4 3))",
	}).Test(t)
}

func TestNthStashBadIndex(t *testing.T) {
	(&sliptest.Function{
		Source:    `(nth-stash t)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}
