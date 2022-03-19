// Copyright (c) 2022, Peter Ohler, All rights reserved.

package test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
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
	scope.Let(slip.Symbol("arg"), slip.Cons{slip.Symbol("a"), slip.Symbol("b")})
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
	scope.Let(slip.Symbol("arg"), slip.List{slip.Symbol("a"), slip.Symbol("b"), slip.Symbol("c")})
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
}
