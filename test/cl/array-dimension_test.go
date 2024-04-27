// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestArrayDimensionVector(t *testing.T) {
	(&sliptest.Function{
		Source: `(array-dimension (make-array 4) 0)`,
		Expect: "4",
	}).Test(t)
}

func TestArrayDimensionArray(t *testing.T) {
	(&sliptest.Function{
		Source: `(array-dimension (make-array '(2 3)) 1)`,
		Expect: "3",
	}).Test(t)
}

func TestArrayDimensionNotArray(t *testing.T) {
	(&sliptest.Function{
		Source:    `(array-dimension t 1)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestArrayDimensionBadAxis(t *testing.T) {
	(&sliptest.Function{
		Source:    `(array-dimension (make-array 4) 1)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(array-dimension (make-array 4) t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
