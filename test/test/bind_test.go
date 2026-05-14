// Copyright (c) 2026, Peter Ohler, All rights reserved.

package test_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestBindOk(t *testing.T) {
	parent := slip.NewScope()
	scope := parent.NewScope()
	(&sliptest.Function{
		Scope:  scope,
		Source: `(progn (bind quux 7) quux)`,
		Expect: "7",
	}).Test(t)
	tt.Equal(t, slip.Fixnum(7), scope.Get(slip.Symbol("quux")))
}

func TestBindBadArgs(t *testing.T) {
	(&sliptest.Function{
		Source:    `(bind quux)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(bind t 7)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
