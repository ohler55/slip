// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestTenthEmpty(t *testing.T) {
	(&sliptest.Function{
		Source: "(tenth nil)",
		Expect: "nil",
	}).Test(t)
}

func TestTenthList(t *testing.T) {
	(&sliptest.Function{
		Source: "(tenth '(a b c d e f g h i j))",
		Expect: "j",
	}).Test(t)
}

func TestTenthCons(t *testing.T) {
	(&sliptest.Function{
		Source: "(tenth '(a b c d e f g h i . j))",
		Expect: "j",
	}).Test(t)
}

func TestTenthNotList(t *testing.T) {
	(&sliptest.Function{
		Source: "(tenth 7)",
		Panics: true,
	}).Test(t)
}

func TestTenthSetfList(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString("(setq target '(a b c d e f g h i j))", scope).Eval(scope, nil)
	(&sliptest.Function{
		Scope:  scope,
		Source: "(setf (tenth target) 'x)",
		Expect: "x",
	}).Test(t)
	tt.Equal(t, "(a b c d e f g h i x)", slip.ObjectString(scope.Get(slip.Symbol("target"))))
}

func TestTenthSetfCons(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString("(setq target '(a b c d e f g h i . j))", scope).Eval(scope, nil)
	(&sliptest.Function{
		Scope:  scope,
		Source: "(setf (tenth target) 'x)",
		Expect: "x",
	}).Test(t)
	tt.Equal(t, "(a b c d e f g h i . x)", slip.ObjectString(scope.Get(slip.Symbol("target"))))
}

func TestTenthSetfNotList(t *testing.T) {
	(&sliptest.Function{
		Source: "(setf (tenth 7) 'x)",
		Panics: true,
	}).Test(t)
}
