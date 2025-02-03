// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestFunctionLambdaExpressionBuiltIn(t *testing.T) {
	(&sliptest.Function{
		Source: `(function-lambda-expression 'list)`,
		Expect: "nil, nil, list",
	}).Test(t)
}

func TestFunctionLambdaExpressionLambda(t *testing.T) {
	(&sliptest.Function{
		Source: `(function-lambda-expression (lambda (x &optional (y 3)) (+ x y)))`,
		Expect: "(lambda (x &optional (y 3)) (+ x y)), nil, nil",
	}).Test(t)
}

func TestFunctionLambdaExpressionLambdaClosure(t *testing.T) {
	(&sliptest.Function{
		Source: `(function-lambda-expression (let ((z 4)) (lambda (x &optional (y 3)) (+ x y))))`,
		Expect: "(lambda (x &optional (y 3)) (+ x y)), t, nil",
	}).Test(t)
}

func TestFunctionLambdaExpressionDefun(t *testing.T) {
	(&sliptest.Function{
		Source: `(defun quux (x) (1+ x))
                 (let ((result (function-lambda-expression 'quux)))
                  (fmakunbound 'quux)
                  result)`,
		Expect: "(lambda (x) (block quux (1+ x))), nil, quux",
	}).Test(t)
}

func TestFunctionLambdaExpressionNotFunc(t *testing.T) {
	(&sliptest.Function{
		Source:    `(function-lambda-expression 7)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(function-lambda-expression 'not-a-function)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
