// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestSecondEmpty(t *testing.T) {
	(&sliptest.Function{
		Source: "(second nil)",
		Expect: "nil",
	}).Test(t)
}

func TestSecondList(t *testing.T) {
	(&sliptest.Function{
		Source: "(second '(a b c))",
		Expect: "b",
	}).Test(t)
}

func TestSecondCons(t *testing.T) {
	(&sliptest.Function{
		Source: "(second '(a . b))",
		Expect: "b",
	}).Test(t)
}

func TestSecondNotList(t *testing.T) {
	(&sliptest.Function{
		Source: "(second 7)",
		Panics: true,
	}).Test(t)
}

func TestSecondSetfList(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString("(setq target '(a b c))").Eval(slip.NewScope(), nil)
	(&sliptest.Function{
		Scope:  scope,
		Source: "(setf (second target) 'x)",
		Expect: "x",
	}).Test(t)
	tt.Equal(t, "(a x c)", slip.ObjectString(scope.Get(slip.Symbol("target"))))
}

func TestSecondSetfCons(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString("(setq target '(a . b))").Eval(slip.NewScope(), nil)
	(&sliptest.Function{
		Scope:  scope,
		Source: "(setf (second target) 'x)",
		Expect: "x",
	}).Test(t)
	tt.Equal(t, "(a . x)", slip.ObjectString(scope.Get(slip.Symbol("target"))))
}

func TestSecondSetfNotList(t *testing.T) {
	(&sliptest.Function{
		Source: "(setf (second 7) 'x)",
		Panics: true,
	}).Test(t)
}
