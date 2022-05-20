// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
	"github.com/stretchr/testify/require"
)

func TestSetfSymbol(t *testing.T) {
	scope := slip.NewScope()
	(&sliptest.Object{
		Scope:  scope,
		Target: slip.NewFunc("setf", slip.List{slip.Fixnum(7), slip.Symbol("x")}),
		String: "(setf x 7)",
		Simple: []interface{}{"setf", "x", 7},
		Eval:   slip.Fixnum(7),
	}).Test(t)
	require.Equal(t, slip.Fixnum(7), scope.Get(slip.Symbol("x")))
}

func TestSetfPlacer(t *testing.T) {
	scope := slip.NewScope()
	scope.Let(slip.Symbol("target"), slip.List{slip.Fixnum(8), slip.Fixnum(7)})
	car := slip.NewFunc("car", slip.List{slip.Symbol("target")})
	(&sliptest.Object{
		Scope:  scope,
		Target: slip.NewFunc("setf", slip.List{slip.Fixnum(9), car}),
		String: "(setf (car target) 9)",
		Simple: []interface{}{"setf", []interface{}{"car", "target"}, 9},
		Eval:   slip.Fixnum(9),
	}).Test(t)
	require.Equal(t, slip.List{slip.Fixnum(8), slip.Fixnum(9)}, scope.Get(slip.Symbol("target")))
}

func TestSetfPlacerList(t *testing.T) {
	scope := slip.NewScope()
	scope.Let(slip.Symbol("target"), slip.List{slip.Fixnum(8), slip.Fixnum(7), slip.Fixnum(6)})
	car := slip.NewFunc("car", slip.List{slip.List{slip.Symbol("target"), slip.Symbol("cdr")}})
	(&sliptest.Object{
		Scope:  scope,
		Target: slip.NewFunc("setf", slip.List{slip.Fixnum(9), car}),
		String: "(setf (car (cdr target)) 9)",
		Simple: []interface{}{"setf", []interface{}{"car", []interface{}{"cdr", "target"}}, 9},
		Eval:   slip.Fixnum(9),
	}).Test(t)
	require.Equal(t, slip.List{slip.Fixnum(8), slip.Fixnum(9), slip.Fixnum(6)}, scope.Get(slip.Symbol("target")))
}

func TestSetfNotPairs(t *testing.T) {
	scope := slip.NewScope()
	(&sliptest.Object{
		Scope:  scope,
		Target: slip.NewFunc("setf", slip.List{slip.Symbol("x")}),
		String: "(setf x)",
		Simple: []interface{}{"setf", "x"},
		Panics: true,
	}).Test(t)
}

func TestSetfNotPlacer(t *testing.T) {
	scope := slip.NewScope()
	(&sliptest.Object{
		Scope:  scope,
		Target: slip.NewFunc("setf", slip.List{slip.Fixnum(8), slip.Fixnum(7)}),
		String: "(setf 7 8)",
		Simple: []interface{}{"setf", 7, 8},
		Panics: true,
	}).Test(t)
}

func TestSetfList(t *testing.T) {
	scope := slip.NewScope()
	scope.Let(slip.Symbol("target"), slip.List{slip.Fixnum(8), slip.Fixnum(7)})
	car := slip.List{slip.Symbol("target"), slip.Symbol("car")}
	(&sliptest.Object{
		Scope:  scope,
		Target: slip.NewFunc("setf", slip.List{slip.Fixnum(9), car}),
		String: "(setf (car target) 9)",
		Simple: []interface{}{"setf", []interface{}{"car", "target"}, 9},
		Eval:   slip.Fixnum(9),
	}).Test(t)
	require.Equal(t, slip.List{slip.Fixnum(8), slip.Fixnum(9)}, scope.Get(slip.Symbol("target")))
}
