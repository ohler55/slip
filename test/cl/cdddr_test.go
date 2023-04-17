// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestCdddrEmpty(t *testing.T) {
	(&sliptest.Function{
		Source: "(cdddr nil)",
		Expect: "nil",
	}).Test(t)
}

func TestCdddrFoundCons(t *testing.T) {
	(&sliptest.Function{
		Source: "(cdddr '(a . (b . (c . d))))",
		Expect: "d",
	}).Test(t)
}

func TestCdddrFound(t *testing.T) {
	(&sliptest.Function{
		Source: "(cdddr '(a b c d))",
		Expect: "(d)",
	}).Test(t)
}

func TestCdddrWrongArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: "(cdddr)",
		Panics: true,
	}).Test(t)
}

func TestCdddrWrongNotList(t *testing.T) {
	(&sliptest.Function{
		Source: "(cdddr t)",
		Panics: true,
	}).Test(t)
}

func TestCdddrSetfCons(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString("(setq target '(a . (b . (c . d))))").Eval(slip.NewScope(), nil)
	(&sliptest.Function{
		Scope:  scope,
		Source: "(setf (cdddr target) 'x)",
		Expect: "x",
	}).Test(t)
	tt.Equal(t, "(a . (b . (c . x)))", slip.ObjectString(scope.Get(slip.Symbol("target"))))
}

func TestCdddrSetfNil(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString("(setq target '(a))").Eval(slip.NewScope(), nil)
	(&sliptest.Function{
		Scope:  scope,
		Source: "(setf (cdddr target) 'x)",
		Panics: true,
	}).Test(t)
}
