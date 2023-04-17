// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestCaaarEmpty(t *testing.T) {
	(&sliptest.Function{
		Source: "(caaar nil)",
		Expect: "nil",
	}).Test(t)
}

func TestCaaarFoundNil(t *testing.T) {
	(&sliptest.Function{
		Source: "(caaar '(nil c))",
		Expect: "nil",
	}).Test(t)
}

func TestCaaarList(t *testing.T) {
	(&sliptest.Function{
		Source: "(caaar '(((a b) c) d))",
		Expect: "a",
	}).Test(t)
}

func TestCaaarCons(t *testing.T) {
	(&sliptest.Function{
		Source: "(caaar '(((a . b) . c) . d))",
		Expect: "a",
	}).Test(t)
}

func TestCaaarWrongArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: "(caaar)",
		Panics: true,
	}).Test(t)
}

func TestCaaarWrongNotList(t *testing.T) {
	(&sliptest.Function{
		Source: "(caaar t)",
		Panics: true,
	}).Test(t)
}

func TestCaaarSetfList(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString("(setq target '(((a b) c) d))").Eval(slip.NewScope(), nil)
	(&sliptest.Function{
		Scope:  scope,
		Source: "(setf (caaar target) 'x)",
		Expect: "x",
	}).Test(t)
	tt.Equal(t, "(((x b) c) d)", slip.ObjectString(scope.Get(slip.Symbol("target"))))
}

func TestCaaarSetfCons(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString("(setq target '(((a . b) . c) . d))").Eval(slip.NewScope(), nil)
	(&sliptest.Function{
		Scope:  scope,
		Source: "(setf (caaar target) 'x)",
		Expect: "x",
	}).Test(t)
	tt.Equal(t, "(((x . b) . c) . d)", slip.ObjectString(scope.Get(slip.Symbol("target"))))
}

func TestCaaarSetfFail(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString("(setq target '(a b))").Eval(slip.NewScope(), nil)
	(&sliptest.Function{
		Scope:  scope,
		Source: "(setf (caaar target) 'x)",
		Panics: true,
	}).Test(t)
}

func TestCaaarSetfArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: "(setf (caaar) 'x)",
		Panics: true,
	}).Test(t)
}
