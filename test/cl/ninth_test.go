// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestNinthEmpty(t *testing.T) {
	(&sliptest.Function{
		Source: "(ninth nil)",
		Expect: "nil",
	}).Test(t)
}

func TestNinthList(t *testing.T) {
	(&sliptest.Function{
		Source: "(ninth '(a b c d e f g h i))",
		Expect: "i",
	}).Test(t)
}

func TestNinthCons(t *testing.T) {
	(&sliptest.Function{
		Source: "(ninth '(a b c d e f g h . i))",
		Expect: "i",
	}).Test(t)
}

func TestNinthNotList(t *testing.T) {
	(&sliptest.Function{
		Source: "(ninth 7)",
		Panics: true,
	}).Test(t)
}

func TestNinthSetfList(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString("(setq target '(a b c d e f g h i))", scope).Eval(scope, nil)
	(&sliptest.Function{
		Scope:  scope,
		Source: "(setf (ninth target) 'x)",
		Expect: "x",
	}).Test(t)
	tt.Equal(t, "(a b c d e f g h x)", slip.ObjectString(scope.Get(slip.Symbol("target"))))
}

func TestNinthSetfCons(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString("(setq target '(a b c d e f g h . i))", scope).Eval(scope, nil)
	(&sliptest.Function{
		Scope:  scope,
		Source: "(setf (ninth target) 'x)",
		Expect: "x",
	}).Test(t)
	tt.Equal(t, "(a b c d e f g h . x)", slip.ObjectString(scope.Get(slip.Symbol("target"))))
}

func TestNinthSetfNotList(t *testing.T) {
	(&sliptest.Function{
		Source: "(setf (ninth 7) 'x)",
		Panics: true,
	}).Test(t)
}
