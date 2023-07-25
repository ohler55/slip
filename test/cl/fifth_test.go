// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestFifthEmpty(t *testing.T) {
	(&sliptest.Function{
		Source: "(fifth nil)",
		Expect: "nil",
	}).Test(t)
}

func TestFifthList(t *testing.T) {
	(&sliptest.Function{
		Source: "(fifth '(a b c d e))",
		Expect: "e",
	}).Test(t)
}

func TestFifthCons(t *testing.T) {
	(&sliptest.Function{
		Source: "(fifth '(a b c d . e))",
		Expect: "e",
	}).Test(t)
}

func TestFifthNotList(t *testing.T) {
	(&sliptest.Function{
		Source: "(fifth 7)",
		Panics: true,
	}).Test(t)
}

func TestFifthSetfList(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString("(setq target '(a b c d e))").Eval(slip.NewScope(), nil)
	(&sliptest.Function{
		Scope:  scope,
		Source: "(setf (fifth target) 'x)",
		Expect: "x",
	}).Test(t)
	tt.Equal(t, "(a b c d x)", slip.ObjectString(scope.Get(slip.Symbol("target"))))
}

func TestFifthSetfCons(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString("(setq target '(a b c d . e))").Eval(slip.NewScope(), nil)
	(&sliptest.Function{
		Scope:  scope,
		Source: "(setf (fifth target) 'x)",
		Expect: "x",
	}).Test(t)
	tt.Equal(t, "(a b c d . x)", slip.ObjectString(scope.Get(slip.Symbol("target"))))
}

func TestFifthSetfNotList(t *testing.T) {
	(&sliptest.Function{
		Source: "(setf (fifth 7) 'x)",
		Panics: true,
	}).Test(t)
}
