// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestArrayRowMajorIndexVector(t *testing.T) {
	(&sliptest.Function{
		Source: `(array-row-major-index (make-array 4) 2)`,
		Expect: "2",
	}).Test(t)
}

func TestArrayRowMajorIndexArray(t *testing.T) {
	(&sliptest.Function{
		Source: `(array-row-major-index (make-array '(2 3)) 1 2)`,
		Expect: "5",
	}).Test(t)
}

func TestArrayRowMajorIndexOctets(t *testing.T) {
	(&sliptest.Function{
		Source: `(array-row-major-index (coerce "abcd" 'octets) 2)`,
		Expect: "2",
	}).Test(t)
	(&sliptest.Function{
		Source:    `(array-row-major-index (coerce "abcd" 'octets) 5)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestArrayRowMajorIndexNotArray(t *testing.T) {
	(&sliptest.Function{
		Source:    `(array-row-major-index t 0 2)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestArrayRowMajorIndexBadSubscript(t *testing.T) {
	(&sliptest.Function{
		Source:    `(array-row-major-index (make-array 2) t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
