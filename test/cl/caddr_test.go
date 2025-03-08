// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestCaddrEmpty(t *testing.T) {
	(&sliptest.Function{
		Source: "(caddr nil)",
		Expect: "nil",
	}).Test(t)
}

func TestCaddrLess(t *testing.T) {
	(&sliptest.Function{
		Source: "(caddr '(a b))",
		Expect: "nil",
	}).Test(t)
}

func TestCaddrFound(t *testing.T) {
	(&sliptest.Function{
		Source: "(caddr '(a b c))",
		Expect: "c",
	}).Test(t)
}

func TestCaddrWrongArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: "(caddr)",
		Panics: true,
	}).Test(t)
}

func TestCaddrWrongNotList(t *testing.T) {
	(&sliptest.Function{
		Source: "(caddr t)",
		Panics: true,
	}).Test(t)
}

func TestCaddrSetfOk(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString("(setq target '(a b c d))", scope).Eval(slip.NewScope(), nil)
	(&sliptest.Function{
		Scope:  scope,
		Source: "(setf (caddr target) 'x)",
		Expect: "x",
	}).Test(t)
	tt.Equal(t, "(a b x d)", slip.ObjectString(scope.Get(slip.Symbol("target"))))
}

func TestCaddrSetfFail(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString("(setq target '(a b))", scope).Eval(slip.NewScope(), nil)
	(&sliptest.Function{
		Scope:  scope,
		Source: "(setf (caddr target) 'x)",
		Panics: true,
	}).Test(t)
}

func TestCaddrSetfArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: "(setf (caddr) 'x)",
		Panics: true,
	}).Test(t)
}
