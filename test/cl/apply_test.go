// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestApplyLambda(t *testing.T) {
	(&sliptest.Function{
		Source: `(apply (lambda (x y) (+ x y)) '(1 2))`,
		Expect: "3",
	}).Test(t)
}

func TestApplySymbol(t *testing.T) {
	(&sliptest.Function{
		Source: `(apply '+ 1 2 '(3 4))`,
		Expect: "10",
	}).Test(t)
}

func TestApplyFunction(t *testing.T) {
	(&sliptest.Function{
		Source: `(apply #'+ 1 2 '())`,
		Expect: "3",
	}).Test(t)
}

func TestApplyNotList(t *testing.T) {
	(&sliptest.Function{
		Source: `(apply '+ 1 2 t)`,
		Panics: true,
	}).Test(t)
}
