// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestSeventhEmpty(t *testing.T) {
	(&sliptest.Function{
		Source: "(seventh nil)",
		Expect: "nil",
	}).Test(t)
}

func TestSeventhList(t *testing.T) {
	(&sliptest.Function{
		Source: "(seventh '(a b c d e f g))",
		Expect: "g",
	}).Test(t)
}

func TestSeventhCons(t *testing.T) {
	(&sliptest.Function{
		Source: "(seventh '(a b c d e f . g))",
		Expect: "g",
	}).Test(t)
}

func TestSeventhNotList(t *testing.T) {
	(&sliptest.Function{
		Source: "(seventh 7)",
		Panics: true,
	}).Test(t)
}

func TestSeventhSetfList(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString("(setq target '(a b c d e f g))").Eval(slip.NewScope(), nil)
	(&sliptest.Function{
		Scope:  scope,
		Source: "(setf (seventh target) 'x)",
		Expect: "x",
	}).Test(t)
	tt.Equal(t, "(a b c d e f x)", slip.ObjectString(scope.Get(slip.Symbol("target"))))
}

func TestSeventhSetfCons(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString("(setq target '(a b c d e f . g))").Eval(slip.NewScope(), nil)
	(&sliptest.Function{
		Scope:  scope,
		Source: "(setf (seventh target) 'x)",
		Expect: "x",
	}).Test(t)
	tt.Equal(t, "(a b c d e f . x)", slip.ObjectString(scope.Get(slip.Symbol("target"))))
}

func TestSeventhSetfNotList(t *testing.T) {
	(&sliptest.Function{
		Source: "(setf (seventh 7) 'x)",
		Panics: true,
	}).Test(t)
}
