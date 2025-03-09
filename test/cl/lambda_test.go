// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/ojg/sen"
	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestLambdaSelf(t *testing.T) {
	scope := slip.NewScope()
	lambda := slip.ReadString("(lambda (x) (car x))", scope).Eval(scope, nil)
	(&sliptest.Object{
		Target:    lambda,
		String:    `/^#<function \(lambda \(x\)\) \{[a-h0-9]+\}>$/`,
		Simple:    sen.MustParse([]byte("[lambda [x] [car x]]")),
		Hierarchy: "lambda.t",
		Equals: []*sliptest.EqTest{
			{Other: slip.True, Expect: false},
		},
		Eval: lambda,
	}).Test(t)
}

func TestLambdaNoArgs(t *testing.T) {
	scope := slip.NewScope()
	lambda := slip.ReadString("(lambda () (terpri))", scope).Eval(scope, nil)
	(&sliptest.Object{
		Target:    lambda,
		String:    `/^#<function \(lambda \(\)\) \{[a-h0-9]+\}>$/`,
		Simple:    sen.MustParse([]byte("[lambda [] [terpri]]")),
		Hierarchy: "lambda.t",
		Equals: []*sliptest.EqTest{
			{Other: slip.True, Expect: false},
		},
		Eval: lambda,
	}).Test(t)
}

func TestLambdaFuncEval(t *testing.T) {
	scope := slip.NewScope()
	result := slip.ReadString("((lambda (x) (car x)) '(1 2 3))", scope).Eval(scope, nil)
	tt.Equal(t, slip.Fixnum(1), result)

	code := slip.ReadString("((lambda (x) (car x)) '(1 2 3))", scope)
	code.Compile()
	tt.Equal(t, 1, len(code))
	tt.Equal(t, `/^\(#<function \(lambda \(x\)\) \{[0-9a-h]+\}> '\(1 2 3\)\)$/`, slip.ObjectString(code[0]))
	tt.SameType(t, &slip.Dynamic{}, code[0])
}

func TestLambdaClosure(t *testing.T) {
	scope := slip.NewScope()
	result := slip.ReadString("(let ((x 3)) ((lambda () x)))", scope).Eval(scope, nil)
	tt.Equal(t, slip.Fixnum(3), result)
}

func TestLambdaBadArgCount(t *testing.T) {
	scope := slip.NewScope()
	tt.Panic(t, func() { _ = slip.ReadString("(lambda)", scope).Eval(scope, nil) })
}
