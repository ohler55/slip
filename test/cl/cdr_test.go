// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
	"github.com/stretchr/testify/require"
)

func TestCdrEmpty(t *testing.T) {
	(&sliptest.Object{
		Target:    slip.NewFunc("cdr", slip.List{nil}),
		String:    "(cdr nil)",
		Simple:    []interface{}{"cdr", nil},
		Hierarchy: "function.t",
		Equals: []*sliptest.EqTest{
			{Other: slip.True, Expect: false},
		},
		Eval: nil,
	}).Test(t)
}

func TestCdrCons(t *testing.T) {
	scope := slip.NewScope()
	scope.Let(slip.Symbol("arg"), slip.Cons{slip.Symbol("b"), slip.Symbol("a")})
	(&sliptest.Object{
		Scope:  scope,
		Target: slip.NewFunc("cdr", slip.List{slip.Symbol("arg")}),
		String: "(cdr arg)",
		Simple: []interface{}{"cdr", "arg"},
		Eval:   slip.Symbol('b'),
	}).Test(t)
}

func TestCdrList(t *testing.T) {
	scope := slip.NewScope()
	scope.Let(slip.Symbol("arg"), slip.List{slip.Symbol("c"), slip.Symbol("b"), slip.Symbol("a")})
	(&sliptest.Object{
		Scope:  scope,
		Target: slip.NewFunc("cdr", slip.List{slip.Symbol("arg")}),
		String: "(cdr arg)",
		Simple: []interface{}{"cdr", "arg"},
		Eval:   slip.List{slip.Symbol('c'), slip.Symbol('b')},
	}).Test(t)
}

func TestCdrBadArg(t *testing.T) {
	scope := slip.NewScope()
	scope.Let(slip.Symbol("arg"), slip.True)
	(&sliptest.Object{
		Scope:  scope,
		Target: slip.NewFunc("cdr", slip.List{slip.Symbol("arg")}),
		String: "(cdr arg)",
		Simple: []interface{}{"cdr", "arg"},
		Panics: true,
	}).Test(t)
}

func TestCdrBadArgCount(t *testing.T) {
	(&sliptest.Object{
		Target: slip.NewFunc("cdr", slip.List{}),
		String: "(cdr)",
		Simple: []interface{}{"cdr"},
		Panics: true,
	}).Test(t)
	(&sliptest.Object{
		Target: slip.NewFunc("cdr", slip.List{nil, nil}),
		String: "(cdr nil nil)",
		Simple: []interface{}{"cdr", nil, nil},
		Panics: true,
	}).Test(t)
}

func TestCdrValues(t *testing.T) {
	code := slip.ReadString(`(cdr (values '(a b) 'c))`)
	scope := slip.NewScope()
	require.Equal(t, slip.List{slip.Symbol("b")}, code.Eval(scope))
}
