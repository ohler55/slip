// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestCadddrEmpty(t *testing.T) {
	(&sliptest.Function{
		Source: "(cadddr nil)",
		Expect: "nil",
	}).Test(t)
}

func TestCadddrLess(t *testing.T) {
	(&sliptest.Function{
		Source: "(cadddr '(a b c))",
		Expect: "nil",
	}).Test(t)
}

func TestCadddrFound(t *testing.T) {
	(&sliptest.Function{
		Source: "(cadddr '(a b c d))",
		Expect: "d",
	}).Test(t)
}

func TestCadddrWrongArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: "(cadddr)",
		Panics: true,
	}).Test(t)
}

func TestCadddrWrongNotList(t *testing.T) {
	(&sliptest.Function{
		Source: "(cadddr t)",
		Panics: true,
	}).Test(t)
}

func TestCadddrSetfOk(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString("(setq target '(a b c d))").Eval(slip.NewScope(), nil)
	(&sliptest.Function{
		Scope:  scope,
		Source: "(setf (cadddr target) 'x)",
		Expect: "x",
	}).Test(t)
	tt.Equal(t, "(a b c x)", slip.ObjectString(scope.Get(slip.Symbol("target"))))
}

func TestCadddrSetfFail(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString("(setq target '(a b))").Eval(slip.NewScope(), nil)
	(&sliptest.Function{
		Scope:  scope,
		Source: "(setf (cadddr target) 'x)",
		Panics: true,
	}).Test(t)
}

func TestCadddrSetfArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: "(setf (cadddr) 'x)",
		Panics: true,
	}).Test(t)
}
