// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestArrayDimensionsVector(t *testing.T) {
	(&sliptest.Function{
		Source: `(array-dimensions (make-array 4))`,
		Expect: "(4)",
	}).Test(t)
}

func TestArrayDimensionsArray(t *testing.T) {
	(&sliptest.Function{
		Source: `(array-dimensions (make-array '(2 3)))`,
		Expect: "(2 3)",
	}).Test(t)
}

func TestArrayDimensionsNotArray(t *testing.T) {
	(&sliptest.Function{
		Source:    `(array-dimensions t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
