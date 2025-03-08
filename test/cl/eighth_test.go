// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestEighthEmpty(t *testing.T) {
	(&sliptest.Function{
		Source: "(eighth nil)",
		Expect: "nil",
	}).Test(t)
}

func TestEighthList(t *testing.T) {
	(&sliptest.Function{
		Source: "(eighth '(a b c d e f g h))",
		Expect: "h",
	}).Test(t)
}

func TestEighthCons(t *testing.T) {
	(&sliptest.Function{
		Source: "(eighth '(a b c d e f g . h))",
		Expect: "h",
	}).Test(t)
}

func TestEighthNotList(t *testing.T) {
	(&sliptest.Function{
		Source: "(eighth 7)",
		Panics: true,
	}).Test(t)
}

func TestEighthSetfList(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString("(setq target '(a b c d e f g h))", scope).Eval(slip.NewScope(), nil)
	(&sliptest.Function{
		Scope:  scope,
		Source: "(setf (eighth target) 'x)",
		Expect: "x",
	}).Test(t)
	tt.Equal(t, "(a b c d e f g x)", slip.ObjectString(scope.Get(slip.Symbol("target"))))
}

func TestEighthSetfCons(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString("(setq target '(a b c d e f g . h))", scope).Eval(slip.NewScope(), nil)
	(&sliptest.Function{
		Scope:  scope,
		Source: "(setf (eighth target) 'x)",
		Expect: "x",
	}).Test(t)
	tt.Equal(t, "(a b c d e f g . x)", slip.ObjectString(scope.Get(slip.Symbol("target"))))
}

func TestEighthSetfNotList(t *testing.T) {
	(&sliptest.Function{
		Source: "(setf (eighth 7) 'x)",
		Panics: true,
	}).Test(t)
}
