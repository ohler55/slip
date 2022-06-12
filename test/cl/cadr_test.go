// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestCadrEmpty(t *testing.T) {
	(&sliptest.Object{
		Target:    slip.NewFunc("cadr", slip.List{nil}),
		String:    "(cadr nil)",
		Simple:    []interface{}{"cadr", nil},
		Hierarchy: "function.t",
		Equals: []*sliptest.EqTest{
			{Other: slip.True, Expect: false},
		},
		Eval: nil,
	}).Test(t)
}

func TestCadrOne(t *testing.T) {
	(&sliptest.Object{
		Target:    slip.NewFunc("cadr", slip.List{slip.NewFunc("quote", slip.List{slip.List{slip.Symbol("a")}})}),
		String:    "(cadr '(a))",
		Simple:    []interface{}{"cadr", []interface{}{"quote", []interface{}{"a"}}},
		Hierarchy: "function.t",
		Eval:      nil,
	}).Test(t)
}

func TestCadrTwo(t *testing.T) {
	(&sliptest.Object{
		Target: slip.NewFunc("cadr",
			slip.List{slip.NewFunc("quote", slip.List{slip.List{slip.Symbol("b"), slip.Symbol("a")}})}),
		String:    "(cadr '(a b))",
		Simple:    []interface{}{"cadr", []interface{}{"quote", []interface{}{"a", "b"}}},
		Hierarchy: "function.t",
		Eval:      slip.Symbol("b"),
	}).Test(t)
}

func TestCadrWrongArgCount(t *testing.T) {
	(&sliptest.Object{
		Target:    slip.NewFunc("cadr", slip.List{}),
		String:    "(cadr)",
		Simple:    []interface{}{"cadr"},
		Hierarchy: "function.t",
		Panics:    true,
	}).Test(t)
}

func TestCadrWrongNotList(t *testing.T) {
	(&sliptest.Object{
		Target:    slip.NewFunc("cadr", slip.List{slip.True}),
		String:    "(cadr t)",
		Simple:    []interface{}{"cadr", true},
		Hierarchy: "function.t",
		Panics:    true,
	}).Test(t)
}

func TestCadrSetf(t *testing.T) {
	scope := slip.NewScope()
	scope.Let(slip.Symbol("target"), slip.List{slip.Fixnum(8), slip.Fixnum(7)})
	cadr := slip.List{slip.Symbol("target"), slip.Symbol("cadr")}
	(&sliptest.Object{
		Scope:  scope,
		Target: slip.NewFunc("setf", slip.List{slip.Fixnum(9), cadr}),
		String: "(setf (cadr target) 9)",
		Simple: []interface{}{"setf", []interface{}{"cadr", "target"}, 9},
		Eval:   slip.Fixnum(9),
	}).Test(t)
	tt.Equal(t, slip.List{slip.Fixnum(9), slip.Fixnum(7)}, scope.Get(slip.Symbol("target")))
}

func TestCadrSetfFail(t *testing.T) {
	scope := slip.NewScope()
	scope.Let(slip.Symbol("target"), slip.List{slip.Fixnum(7)})
	cadr := slip.List{slip.Symbol("target"), slip.Symbol("cadr")}
	(&sliptest.Object{
		Scope:  scope,
		Target: slip.NewFunc("setf", slip.List{slip.Fixnum(9), cadr}),
		String: "(setf (cadr target) 9)",
		Simple: []interface{}{"setf", []interface{}{"cadr", "target"}, 9},
		Panics: true,
	}).Test(t)
}

func TestCadrSetfNoArg(t *testing.T) {
	scope := slip.NewScope()
	scope.Let(slip.Symbol("target"), slip.List{})
	cadr := slip.List{slip.Symbol("cadr")}
	(&sliptest.Object{
		Scope:  scope,
		Target: slip.NewFunc("setf", slip.List{slip.Fixnum(9), cadr}),
		String: "(setf (cadr) 9)",
		Simple: []interface{}{"setf", []interface{}{"cadr"}, 9},
		Panics: true,
	}).Test(t)
}

func TestCadrValues(t *testing.T) {
	code := slip.ReadString(`(cadr (values '(a b) 'c))`)
	scope := slip.NewScope()
	tt.Equal(t, slip.Symbol("b"), code.Eval(scope))
}
