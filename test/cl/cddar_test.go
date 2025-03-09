// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestCddarList(t *testing.T) {
	(&sliptest.Function{
		Source: "(cddar '((a b c)))",
		Expect: "(c)",
	}).Test(t)
}

func TestCddarCons(t *testing.T) {
	(&sliptest.Function{
		Source: "(cddar '((a . (b . c))))",
		Expect: "c",
	}).Test(t)
}

func TestCddarSetf(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString("(setq target '((a . (b . c))))", scope).Eval(slip.NewScope(), nil)
	(&sliptest.Function{
		Scope:  scope,
		Source: "(setf (cddar target) 'x)",
		Expect: "x",
	}).Test(t)
	tt.Equal(t, "((a . (b . x)))", slip.ObjectString(scope.Get(slip.Symbol("target"))))
}
