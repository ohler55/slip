// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestCaaaarEmpty(t *testing.T) {
	(&sliptest.Function{
		Source: "(caaaar nil)",
		Expect: "nil",
	}).Test(t)
}

func TestCaaaarFoundNil(t *testing.T) {
	(&sliptest.Function{
		Source: "(caaaar '(nil c))",
		Expect: "nil",
	}).Test(t)
}

func TestCaaaarFound(t *testing.T) {
	(&sliptest.Function{
		Source: "(caaaar '((((a b) c) d) e))",
		Expect: "a",
	}).Test(t)
}

func TestCaaaarCons(t *testing.T) {
	(&sliptest.Function{
		Source: "(caaaar '((((a . b) . c) . d) . e))",
		Expect: "a",
	}).Test(t)
}

func TestCaaaarWrongArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: "(caaaar)",
		Panics: true,
	}).Test(t)
}

func TestCaaaarWrongNotList(t *testing.T) {
	(&sliptest.Function{
		Source: "(caaaar t)",
		Panics: true,
	}).Test(t)
}

func TestCaaaarSetfOk(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString("(setq target '((((a b) c) d) e))").Eval(slip.NewScope())
	(&sliptest.Function{
		Scope:  scope,
		Source: "(setf (caaaar target) 'x)",
		Expect: "x",
	}).Test(t)
	tt.Equal(t, "((((x b) c) d) e)", slip.ObjectString(scope.Get(slip.Symbol("target"))))
}

func TestCaaaarSetfFail(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString("(setq target '(a b))").Eval(slip.NewScope())
	(&sliptest.Function{
		Scope:  scope,
		Source: "(setf (caaaar target) 'x)",
		Panics: true,
	}).Test(t)
}

func TestCaaaarSetfArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: "(setf (caaaar) 'x)",
		Panics: true,
	}).Test(t)
}
