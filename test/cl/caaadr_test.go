// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestCaaadrEmpty(t *testing.T) {
	(&sliptest.Function{
		Source: "(caaadr nil)",
		Expect: "nil",
	}).Test(t)
}

func TestCaaadrFoundNil(t *testing.T) {
	(&sliptest.Function{
		Source: "(caaadr '(a nil c))",
		Expect: "nil",
	}).Test(t)
}

func TestCaaadrFound(t *testing.T) {
	(&sliptest.Function{
		Source: "(caaadr '(a ((b c) d) e))",
		Expect: "b",
	}).Test(t)
}

func TestCaaadrWrongArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: "(caaadr)",
		Panics: true,
	}).Test(t)
}

func TestCaaadrWrongNotList(t *testing.T) {
	(&sliptest.Function{
		Source: "(caaadr t)",
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: "(caaadr '(a (b c)))",
		Panics: true,
	}).Test(t)
}

func TestCaaadrSetfOk(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString("(setq target '(a ((b c) d) e))", scope).Eval(slip.NewScope(), nil)
	(&sliptest.Function{
		Scope:  scope,
		Source: "(setf (caaadr target) 'x)",
		Expect: "x",
	}).Test(t)
	tt.Equal(t, "(a ((x c) d) e)", slip.ObjectString(scope.Get(slip.Symbol("target"))))
}

func TestCaaadrSetfFail(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString("(setq target '(a (b c)))", scope).Eval(slip.NewScope(), nil)
	(&sliptest.Function{
		Scope:  scope,
		Source: "(setf (caaadr target) 'x)",
		Panics: true,
	}).Test(t)
}

func TestCaaadrSetfArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: "(setf (caaadr) 'x)",
		Panics: true,
	}).Test(t)
}
