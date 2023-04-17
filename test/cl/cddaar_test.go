// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestCddaarList(t *testing.T) {
	(&sliptest.Function{
		Source: "(cddaar '(((a b c))))",
		Expect: "(c)",
	}).Test(t)
}

func TestCddaarCons(t *testing.T) {
	(&sliptest.Function{
		Source: "(cddaar '(((a . (b . c)))))",
		Expect: "c",
	}).Test(t)
}

func TestCddaarSetf(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString("(setq target '(((a . (b . c)))))").Eval(slip.NewScope(), nil)
	(&sliptest.Function{
		Scope:  scope,
		Source: "(setf (cddaar target) 'x)",
		Expect: "x",
	}).Test(t)
	tt.Equal(t, "(((a . (b . x))))", slip.ObjectString(scope.Get(slip.Symbol("target"))))
}
