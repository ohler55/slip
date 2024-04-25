// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestArrayHasFillPointerPVector(t *testing.T) {
	(&sliptest.Function{
		Source: `(array-has-fill-pointer-p (make-array 4))`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: `(array-has-fill-pointer-p (make-array 4 :fill-pointer 2))`,
		Expect: "t",
	}).Test(t)
}

func TestArrayHasFillPointerPArray(t *testing.T) {
	(&sliptest.Function{
		Source: `(array-has-fill-pointer-p (make-array '(2 3)))`,
		Expect: "nil",
	}).Test(t)
}

func TestArrayHasFillPointerPNotArray(t *testing.T) {
	(&sliptest.Function{
		Source:    `(array-has-fill-pointer-p t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
