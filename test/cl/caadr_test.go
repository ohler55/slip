// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestCaadrEmpty(t *testing.T) {
	(&sliptest.Function{
		Source: "(caadr nil)",
		Expect: "nil",
	}).Test(t)
}

func TestCaadrFoundNil(t *testing.T) {
	(&sliptest.Function{
		Source: "(caadr '(a nil c))",
		Expect: "nil",
	}).Test(t)
}

func TestCaadrFound(t *testing.T) {
	(&sliptest.Function{
		Source: "(caadr '(a (b c) d))",
		Expect: "b",
	}).Test(t)
}

func TestCaadrWrongArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: "(caadr)",
		Panics: true,
	}).Test(t)
}

func TestCaadrWrongNotList(t *testing.T) {
	(&sliptest.Function{
		Source: "(caadr t)",
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: "(caadr '(a b))",
		Panics: true,
	}).Test(t)
}

func TestCaadrSetfOk(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString("(setq target '(a (b c) d))").Eval(slip.NewScope(), nil)
	(&sliptest.Function{
		Scope:  scope,
		Source: "(setf (caadr target) 'x)",
		Expect: "x",
	}).Test(t)
	tt.Equal(t, "(a (x c) d)", slip.ObjectString(scope.Get(slip.Symbol("target"))))
}

func TestCaadrSetfFail(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString("(setq target '(a b))").Eval(slip.NewScope(), nil)
	(&sliptest.Function{
		Scope:  scope,
		Source: "(setf (caadr target) 'x)",
		Panics: true,
	}).Test(t)
}

func TestCaadrSetfArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: "(setf (caadr) 'x)",
		Panics: true,
	}).Test(t)
}
