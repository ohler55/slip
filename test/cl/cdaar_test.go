// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestCdaarList(t *testing.T) {
	(&sliptest.Function{
		Source: "(cdaar '(((a b c))))",
		Expect: "(b c)",
	}).Test(t)
}

func TestCdaarCons(t *testing.T) {
	(&sliptest.Function{
		Source: "(cdaar '(((a . b))))",
		Expect: "b",
	}).Test(t)
}

func TestCdaarSetf(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString("(setq target '(((a . b))))", scope).Eval(slip.NewScope(), nil)
	(&sliptest.Function{
		Scope:  scope,
		Source: "(setf (cdaar target) 'x)",
		Expect: "x",
	}).Test(t)
	tt.Equal(t, "(((a . x)))", slip.ObjectString(scope.Get(slip.Symbol("target"))))
}
