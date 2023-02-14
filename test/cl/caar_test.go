// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestCaarEmpty(t *testing.T) {
	(&sliptest.Function{
		Source: "(caar nil)",
		Expect: "nil",
	}).Test(t)
}

func TestCaarFoundNil(t *testing.T) {
	(&sliptest.Function{
		Source: "(caar '(nil c))",
		Expect: "nil",
	}).Test(t)
}

func TestCaarList(t *testing.T) {
	(&sliptest.Function{
		Source: "(caar '((a b) c))",
		Expect: "a",
	}).Test(t)
}

func TestCaarCons(t *testing.T) {
	(&sliptest.Function{
		Source: "(caar '((a . b) . c))",
		Expect: "a",
	}).Test(t)
}

func TestCaarWrongArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: "(caar)",
		Panics: true,
	}).Test(t)
}

func TestCaarWrongNotList(t *testing.T) {
	(&sliptest.Function{
		Source: "(caar t)",
		Panics: true,
	}).Test(t)
}

func TestCaarSetfList(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString("(setq target '((a b) c))").Eval(slip.NewScope())
	(&sliptest.Function{
		Scope:  scope,
		Source: "(setf (caar target) 'x)",
		Expect: "x",
	}).Test(t)
	tt.Equal(t, "((x b) c)", slip.ObjectString(scope.Get(slip.Symbol("target"))))
}

func TestCaarSetfCons(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString("(setq target '((a . b) . c))").Eval(slip.NewScope())
	(&sliptest.Function{
		Scope:  scope,
		Source: "(setf (caar target) 'x)",
		Expect: "x",
	}).Test(t)
	tt.Equal(t, "((x . b) . c)", slip.ObjectString(scope.Get(slip.Symbol("target"))))
}

func TestCaarSetfFail(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString("(setq target '(a b))").Eval(slip.NewScope())
	(&sliptest.Function{
		Scope:  scope,
		Source: "(setf (caar target) 'x)",
		Panics: true,
	}).Test(t)
}

func TestCaarSetfArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: "(setf (caar) 'x)",
		Panics: true,
	}).Test(t)
}
