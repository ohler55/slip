// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestCadarList(t *testing.T) {
	(&sliptest.Function{
		Source: "(cadar '((a b c)))",
		Expect: "b",
	}).Test(t)
}

func TestCadarCons(t *testing.T) {
	(&sliptest.Function{
		Source: "(cadar '((a . (b . c))))",
		Expect: "b",
	}).Test(t)
}

func TestCadarSetf(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString("(setq target '((a b c)))").Eval(slip.NewScope())
	(&sliptest.Function{
		Scope:  scope,
		Source: "(setf (cadar target) 'x)",
		Expect: "x",
	}).Test(t)
	tt.Equal(t, "((a x c))", slip.ObjectString(scope.Get(slip.Symbol("target"))))
}
