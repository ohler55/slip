// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestFourthEmpty(t *testing.T) {
	(&sliptest.Function{
		Source: "(fourth nil)",
		Expect: "nil",
	}).Test(t)
}

func TestFourthList(t *testing.T) {
	(&sliptest.Function{
		Source: "(fourth '(a b c d))",
		Expect: "d",
	}).Test(t)
}

func TestFourthCons(t *testing.T) {
	(&sliptest.Function{
		Source: "(fourth '(a b c . d))",
		Expect: "d",
	}).Test(t)
}

func TestFourthNotList(t *testing.T) {
	(&sliptest.Function{
		Source: "(fourth 7)",
		Panics: true,
	}).Test(t)
}

func TestFourthSetfList(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString("(setq target '(a b c d))").Eval(slip.NewScope(), nil)
	(&sliptest.Function{
		Scope:  scope,
		Source: "(setf (fourth target) 'x)",
		Expect: "x",
	}).Test(t)
	tt.Equal(t, "(a b c x)", slip.ObjectString(scope.Get(slip.Symbol("target"))))
}

func TestFourthSetfCons(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString("(setq target '(a b c . d))").Eval(slip.NewScope(), nil)
	(&sliptest.Function{
		Scope:  scope,
		Source: "(setf (fourth target) 'x)",
		Expect: "x",
	}).Test(t)
	tt.Equal(t, "(a b c . x)", slip.ObjectString(scope.Get(slip.Symbol("target"))))
}

func TestFourthSetfNotList(t *testing.T) {
	(&sliptest.Function{
		Source: "(setf (fourth 7) 'x)",
		Panics: true,
	}).Test(t)
}
