// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestSetqBasic(t *testing.T) {
	scope := slip.NewScope()
	(&sliptest.Object{
		Scope:  scope,
		Target: slip.NewFunc("setq", slip.List{slip.Symbol("x"), slip.Fixnum(7)}),
		String: "(setq x 7)",
		Simple: []interface{}{"setq", "x", 7},
		Eval:   slip.Fixnum(7),
	}).Test(t)
	tt.Equal(t, slip.Fixnum(7), scope.Get(slip.Symbol("x")))
}

func TestSetqEmpty(t *testing.T) {
	(&sliptest.Object{
		Target: slip.NewFunc("setq", slip.List{}),
		String: "(setq)",
		Simple: []interface{}{"setq"},
		Eval:   nil,
	}).Test(t)
}

func TestSetqMultiple(t *testing.T) {
	scope := slip.NewScope()
	(&sliptest.Object{
		Scope:  scope,
		Target: slip.NewFunc("setq", slip.List{slip.Symbol("x"), slip.Fixnum(7), slip.Symbol("y"), slip.Fixnum(8)}),
		String: "(setq x 7 y 8)",
		Simple: []interface{}{"setq", "x", 7, "y", 8},
		Eval:   slip.Fixnum(8),
	}).Test(t)
	tt.Equal(t, slip.Fixnum(7), scope.Get(slip.Symbol("x")))
	tt.Equal(t, slip.Fixnum(8), scope.Get(slip.Symbol("y")))
}

func TestSetqUpdateFunc(t *testing.T) {
	list := slip.NewFunc("quote", slip.List{slip.List{slip.Fixnum(7)}})
	scope := slip.NewScope()
	(&sliptest.Object{
		Scope:  scope,
		Target: slip.NewFunc("setq", slip.List{slip.Symbol("x"), slip.List{slip.Symbol("car"), list}}),
		String: "(setq x (car '(7)))",
		Simple: []interface{}{"setq", "x", []interface{}{"car", []interface{}{"quote", []interface{}{7}}}},
		Eval:   slip.Fixnum(7),
	}).Test(t)
	tt.Equal(t, slip.Fixnum(7), scope.Get(slip.Symbol("x")))
}

func TestSetqBadArgCount(t *testing.T) {
	scope := slip.NewScope()
	(&sliptest.Object{
		Scope:  scope,
		Target: slip.NewFunc("setq", slip.List{slip.Symbol("x")}),
		String: "(setq x)",
		Simple: []interface{}{"setq", "x"},
		Panics: true,
	}).Test(t)
}

func TestSetqBadSymbol(t *testing.T) {
	(&sliptest.Object{
		Target: slip.NewFunc("setq", slip.List{slip.True, slip.True}),
		String: "(setq t t)",
		Simple: []interface{}{"setq", true, true},
		Panics: true,
	}).Test(t)
}
