// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestCadrEmpty(t *testing.T) {
	(&sliptest.Function{
		Source: "(cadr nil)",
		Expect: "nil",
	}).Test(t)
}

func TestCadrFoundNil(t *testing.T) {
	(&sliptest.Function{
		Source: "(cadr '(nil))",
		Expect: "nil",
	}).Test(t)
}

func TestCadrFound(t *testing.T) {
	(&sliptest.Function{
		Source: "(cadr '(a b c))",
		Expect: "b",
	}).Test(t)
}

func TestCadrWrongArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: "(cadr)",
		Panics: true,
	}).Test(t)
}

func TestCadrWrongNotList(t *testing.T) {
	(&sliptest.Function{
		Source: "(cadr t)",
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: "(cadr '(a . b))",
		Panics: true,
	}).Test(t)
}

func TestCadrSetfOk(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString("(setq target '(a b c))").Eval(slip.NewScope(), nil)
	(&sliptest.Function{
		Scope:  scope,
		Source: "(setf (cadr target) 'x)",
		Expect: "x",
	}).Test(t)
	tt.Equal(t, "(a x c)", slip.ObjectString(scope.Get(slip.Symbol("target"))))
}

func TestCadrSetfFail(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString("(setq target '(a . b))").Eval(slip.NewScope(), nil)
	(&sliptest.Function{
		Scope:  scope,
		Source: "(setf (cadr target) 'x)",
		Panics: true,
	}).Test(t)
}

func TestCadrSetfArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: "(setf (cadr) 'x)",
		Panics: true,
	}).Test(t)
}
