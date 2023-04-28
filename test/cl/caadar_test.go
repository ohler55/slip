// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestCaadarList(t *testing.T) {
	(&sliptest.Function{
		Source: "(caadar '((a (b c d))))",
		Expect: "b",
	}).Test(t)
}

func TestCaadarCons(t *testing.T) {
	(&sliptest.Function{
		Source: "(caadar '((a . ((b . c))) . d))",
		Expect: "b",
	}).Test(t)
}

func TestCaadarSetf(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString("(setq target '((a . ((b . c)))))").Eval(slip.NewScope(), nil)
	(&sliptest.Function{
		Scope:  scope,
		Source: "(setf (caadar target) 'x)",
		Expect: "x",
	}).Test(t)
	tt.Equal(t, "((a . ((x . c))))", slip.ObjectString(scope.Get(slip.Symbol("target"))))
}
