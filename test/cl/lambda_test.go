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
	lambda := slip.ReadEvalString("(lambda (x) (car x))", scope)
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
	lambda := slip.ReadEvalString("(lambda () (terpri))", scope)
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
	result := slip.ReadEvalString("((lambda (x) (car x)) '(1 2 3))", scope)
	tt.Equal(t, slip.Fixnum(1), result)

	code := slip.ReadString("((lambda (x) (car x)) '(1 2 3))", scope)
	code.Compile()
	tt.Equal(t, 1, len(code))
	tt.Equal(t, `/^\(#<function \(lambda \(x\)\) \{[0-9a-h]+\}> '\(1 2 3\)\)$/`, slip.ObjectString(code[0]))
	tt.SameType(t, &slip.Dynamic{}, code[0])
}

func TestLambdaClosure(t *testing.T) {
	scope := slip.NewScope()
	result := slip.ReadEvalString("(let ((x 3)) ((lambda () x)))", scope)
	tt.Equal(t, slip.Fixnum(3), result)
}

func TestLambdaBadArgCount(t *testing.T) {
	scope := slip.NewScope()
	tt.Panic(t, func() { _ = slip.ReadEvalString("(lambda)", scope) })
}
