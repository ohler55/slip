// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestFirstEmpty(t *testing.T) {
	(&sliptest.Function{
		Source: "(first nil)",
		Expect: "nil",
	}).Test(t)
}

func TestFirstList(t *testing.T) {
	(&sliptest.Function{
		Source: "(first '(a b c))",
		Expect: "a",
	}).Test(t)
}

func TestFirstCons(t *testing.T) {
	(&sliptest.Function{
		Source: "(first '(a . b))",
		Expect: "a",
	}).Test(t)
}

func TestFirstNotList(t *testing.T) {
	(&sliptest.Function{
		Source: "(first 7)",
		Panics: true,
	}).Test(t)
}

func TestFirstSetfOk(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString("(setq target '(a b c))", scope).Eval(slip.NewScope(), nil)
	(&sliptest.Function{
		Scope:  scope,
		Source: "(setf (first target) 'x)",
		Expect: "x",
	}).Test(t)
	tt.Equal(t, "(x b c)", slip.ObjectString(scope.Get(slip.Symbol("target"))))
}

func TestFirstSetfNotList(t *testing.T) {
	(&sliptest.Function{
		Source: "(setf (first 7) 'x)",
		Panics: true,
	}).Test(t)
}
