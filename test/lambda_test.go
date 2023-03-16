// Copyright (c) 2023, Peter Ohler, All rights reserved.

package test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestDefLambdaNoArgs(t *testing.T) {
	lam := slip.DefLambda("deflambda-test", nil, slip.List{slip.List{}})
	(&sliptest.Object{
		Target:    lam,
		String:    `/^#<function \(lambda \(\)\) {[0-9a-f]+}>/`,
		Simple:    []any{"lambda", []any{}, []any{}},
		Hierarchy: "lambda.t",
		Equals: []*sliptest.EqTest{
			{Other: lam, Expect: true},
			{Other: slip.True, Expect: false},
		},
		Eval: lam,
	}).Test(t)
}

func TestDefLambdaArgs(t *testing.T) {
	lam := slip.DefLambda("deflambda-test", nil,
		slip.List{
			slip.List{slip.Symbol("x"), slip.List{slip.Symbol("y"), nil}},
			slip.String("docs"),
			slip.List{slip.Symbol("1+"), slip.Fixnum(1)},
		})
	(&sliptest.Object{
		Target:    lam,
		String:    `/^#<function \(lambda \(x y\)\) {[0-9a-f]+}>/`,
		Simple:    []any{"lambda", []any{"x", "y"}, []any{"1+", 1}},
		Hierarchy: "lambda.t",
		Equals: []*sliptest.EqTest{
			{Other: lam, Expect: true},
			{Other: slip.True, Expect: false},
		},
		Eval: lam,
	}).Test(t)
}

func TestDefLambdaArgsNotList(t *testing.T) {
	tt.Panic(t, func() { slip.DefLambda("deflambda-test", nil, slip.List{slip.True}) })
}

func TestDefLambdaBadArgType(t *testing.T) {
	tt.Panic(t, func() { slip.DefLambda("deflambda-test", nil, slip.List{slip.List{slip.True}}) })
}

func TestDefLambdaBadArgList(t *testing.T) {
	tt.Panic(t, func() { slip.DefLambda("deflambda-test", nil, slip.List{slip.List{slip.List{slip.Symbol("x")}}}) })
	tt.Panic(t, func() { slip.DefLambda("deflambda-test", nil, slip.List{slip.List{slip.List{slip.True, slip.True}}}) })
}

func TestLambdaCallWithScope(t *testing.T) {
	scope := slip.NewScope()
	_ = slip.ReadString(`(let ((y 3)) (setq call-it (lambda (x) (+ x y))))`).Eval(scope)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(apply call-it '(2))`,
		Expect: "5",
	}).Test(t)
}

func TestLambdaCallBadKeyword(t *testing.T) {
	(&sliptest.Function{
		Source: `((lambda (&key :test) test) :test)`,
		Panics: true,
	}).Test(t)
}

func TestLambdaCallTooManyArgs(t *testing.T) {
	(&sliptest.Function{
		Source: `((lambda () nil) 5)`,
		Panics: true,
	}).Test(t)
}

/*
func TestLambdaCallReturnNoTag(t *testing.T) {
	(&sliptest.Function{
		Source: `((lambda (x) (return 3) x) 5)`,
		Expect: "3",
	}).Test(t)
}
*/
