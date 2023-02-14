// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestCdadrList(t *testing.T) {
	(&sliptest.Function{
		Source: "(cdadr '(a (b c)))",
		Expect: "(c)",
	}).Test(t)
}

func TestCdadrCons(t *testing.T) {
	(&sliptest.Function{
		Source: "(cdadr '(a (b . c)))",
		Expect: "c",
	}).Test(t)
}

func TestCdadrSetf(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString("(setq target '(a (b . c)))").Eval(slip.NewScope())
	(&sliptest.Function{
		Scope:  scope,
		Source: "(setf (cdadr target) 'x)",
		Expect: "x",
	}).Test(t)
	tt.Equal(t, "(a (b . x))", slip.ObjectString(scope.Get(slip.Symbol("target"))))
}
