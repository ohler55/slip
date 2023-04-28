// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestCadaarList(t *testing.T) {
	(&sliptest.Function{
		Source: "(cadaar '(((a b c))))",
		Expect: "b",
	}).Test(t)
}

func TestCadaarCons(t *testing.T) {
	(&sliptest.Function{
		Source: "(cadaar '(((a . (b . c)) . d) . e))",
		Expect: "b",
	}).Test(t)
}

func TestCadaarSetf(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString("(setq target '(((a . (b . c)) . d) . e))").Eval(slip.NewScope(), nil)
	(&sliptest.Function{
		Scope:  scope,
		Source: "(setf (cadaar target) 'x)",
		Expect: "x",
	}).Test(t)
	tt.Equal(t, "(((a . (x . c)) . d) . e)", slip.ObjectString(scope.Get(slip.Symbol("target"))))
}
