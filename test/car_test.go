// Copyright (c) 2022, Peter Ohler, All rights reserved.

package test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
	"github.com/stretchr/testify/require"
)

func TestCarEmpty(t *testing.T) {
	(&sliptest.Object{
		Target:    slip.NewFunc("car", slip.List{nil}),
		String:    "(car nil)",
		Simple:    []interface{}{"car", nil},
		Hierarchy: "function.t",
		Equals: []*sliptest.EqTest{
			{Other: slip.True, Expect: false},
		},
		Eval: nil,
	}).Test(t)
}

func TestCarCons(t *testing.T) {
	scope := slip.NewScope()
	scope.Let(slip.Symbol("arg"), slip.Cons{slip.Symbol("b"), slip.Symbol("a")})
	(&sliptest.Object{
		Scope:  scope,
		Target: slip.NewFunc("car", slip.List{slip.Symbol("arg")}),
		String: "(car arg)",
		Simple: []interface{}{"car", "arg"},
		Eval:   slip.Symbol('a'),
	}).Test(t)
}

func TestCarList(t *testing.T) {
	scope := slip.NewScope()
	scope.Let(slip.Symbol("arg"), slip.List{slip.Symbol("c"), slip.Symbol("b"), slip.Symbol("a")})
	(&sliptest.Object{
		Scope:  scope,
		Target: slip.NewFunc("car", slip.List{slip.Symbol("arg")}),
		String: "(car arg)",
		Simple: []interface{}{"car", "arg"},
		Eval:   slip.Symbol('a'),
	}).Test(t)
}

func TestCarBadArg(t *testing.T) {
	scope := slip.NewScope()
	scope.Let(slip.Symbol("arg"), slip.True)
	(&sliptest.Object{
		Scope:  scope,
		Target: slip.NewFunc("car", slip.List{slip.Symbol("arg")}),
		String: "(car arg)",
		Simple: []interface{}{"car", "arg"},
		Panics: true,
	}).Test(t)
}

func TestCarBadArgCount(t *testing.T) {
	(&sliptest.Object{
		Target: slip.NewFunc("car", slip.List{}),
		String: "(car)",
		Simple: []interface{}{"car"},
		Panics: true,
	}).Test(t)
	(&sliptest.Object{
		Target: slip.NewFunc("car", slip.List{nil, nil}),
		String: "(car nil nil)",
		Simple: []interface{}{"car", nil, nil},
		Panics: true,
	}).Test(t)
}

func TestCarSetfCons(t *testing.T) {
	scope := slip.NewScope()
	scope.Let(slip.Symbol("target"), slip.Cons{slip.Fixnum(8), slip.Fixnum(7)})
	car := slip.List{slip.Symbol("target"), slip.Symbol("car")}
	(&sliptest.Object{
		Scope:  scope,
		Target: slip.NewFunc("setf", slip.List{slip.Fixnum(9), car}),
		String: "(setf (car target) 9)",
		Simple: []interface{}{"setf", []interface{}{"car", "target"}, 9},
		Eval:   slip.Fixnum(9),
	}).Test(t)
	require.Equal(t, slip.Cons{slip.Fixnum(8), slip.Fixnum(9)}, scope.Get(slip.Symbol("target")))
}

func TestCarSetfNoArg(t *testing.T) {
	scope := slip.NewScope()
	scope.Let(slip.Symbol("target"), slip.List{})
	car := slip.List{slip.Symbol("car")}
	(&sliptest.Object{
		Scope:  scope,
		Target: slip.NewFunc("setf", slip.List{slip.Fixnum(9), car}),
		String: "(setf (car) 9)",
		Simple: []interface{}{"setf", []interface{}{"car"}, 9},
		Panics: true,
	}).Test(t)
}

func TestCarSetfNotList(t *testing.T) {
	scope := slip.NewScope()
	car := slip.List{slip.True, slip.Symbol("car")}
	(&sliptest.Object{
		Scope:  scope,
		Target: slip.NewFunc("setf", slip.List{slip.Fixnum(9), car}),
		String: "(setf (car t) 9)",
		Simple: []interface{}{"setf", []interface{}{"car", true}, 9},
		Panics: true,
	}).Test(t)
}
