// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestSixthEmpty(t *testing.T) {
	(&sliptest.Function{
		Source: "(sixth nil)",
		Expect: "nil",
	}).Test(t)
}

func TestSixthList(t *testing.T) {
	(&sliptest.Function{
		Source: "(sixth '(a b c d e f))",
		Expect: "f",
	}).Test(t)
}

func TestSixthCons(t *testing.T) {
	(&sliptest.Function{
		Source: "(sixth '(a b c d e . f))",
		Expect: "f",
	}).Test(t)
}

func TestSixthNotList(t *testing.T) {
	(&sliptest.Function{
		Source: "(sixth 7)",
		Panics: true,
	}).Test(t)
}

func TestSixthSetfList(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString("(setq target '(a b c d e f))").Eval(slip.NewScope(), nil)
	(&sliptest.Function{
		Scope:  scope,
		Source: "(setf (sixth target) 'x)",
		Expect: "x",
	}).Test(t)
	tt.Equal(t, "(a b c d e x)", slip.ObjectString(scope.Get(slip.Symbol("target"))))
}

func TestSixthSetfCons(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString("(setq target '(a b c d e . f))").Eval(slip.NewScope(), nil)
	(&sliptest.Function{
		Scope:  scope,
		Source: "(setf (sixth target) 'x)",
		Expect: "x",
	}).Test(t)
	tt.Equal(t, "(a b c d e . x)", slip.ObjectString(scope.Get(slip.Symbol("target"))))
}

func TestSixthSetfNotList(t *testing.T) {
	(&sliptest.Function{
		Source: "(setf (sixth 7) 'x)",
		Panics: true,
	}).Test(t)
}
