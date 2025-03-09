// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestCdaddrList(t *testing.T) {
	(&sliptest.Function{
		Source: "(cdaddr '(a b (c d)))",
		Expect: "(d)",
	}).Test(t)
}

func TestCdaddrCons(t *testing.T) {
	(&sliptest.Function{
		Source: "(cdaddr '(a b (c . d)))",
		Expect: "d",
	}).Test(t)
}

func TestCdaddrSetf(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString("(setq target '(a b (c . d)))", scope).Eval(slip.NewScope(), nil)
	(&sliptest.Function{
		Scope:  scope,
		Source: "(setf (cdaddr target) 'x)",
		Expect: "x",
	}).Test(t)
	tt.Equal(t, "(a b (c . x))", slip.ObjectString(scope.Get(slip.Symbol("target"))))
}
