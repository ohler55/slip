// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestFuncallLambda(t *testing.T) {
	(&sliptest.Function{
		Source: `(funcall (lambda (x y) (+ x y)) 1 2)`,
		Expect: "3",
	}).Test(t)
}

func TestFuncallSymbol(t *testing.T) {
	(&sliptest.Function{
		Source: `(funcall '+ 1 2 3 4)`,
		Expect: "10",
	}).Test(t)
}

func TestFuncallFunction(t *testing.T) {
	(&sliptest.Function{
		Source: `(funcall #'+ 1 2)`,
		Expect: "3",
	}).Test(t)
}
