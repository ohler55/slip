// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestArrayDisplacementVector(t *testing.T) {
	(&sliptest.Function{
		Source: `(array-displacement (make-array 4))`,
		Expect: "nil, 0",
	}).Test(t)
}

func TestArrayDisplacementArray(t *testing.T) {
	(&sliptest.Function{
		Source: `(array-displacement (make-array '(2 3)))`,
		Expect: "nil, 0",
	}).Test(t)
}

func TestArrayDisplacementNotArray(t *testing.T) {
	(&sliptest.Function{
		Source:    `(array-displacement t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
