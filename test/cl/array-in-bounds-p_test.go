// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestArrayInBoundsPVector(t *testing.T) {
	(&sliptest.Function{
		Source: `(array-in-bounds-p (make-array 4) 2)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(array-in-bounds-p (make-array 4) 5)`,
		Expect: "nil",
	}).Test(t)
}

func TestArrayInBoundsPArray(t *testing.T) {
	(&sliptest.Function{
		Source: `(array-in-bounds-p (make-array '(2 3)) 1 2)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(array-in-bounds-p (make-array '(2 3)) 1 3)`,
		Expect: "nil",
	}).Test(t)
}

func TestArrayInBoundsPOctets(t *testing.T) {
	(&sliptest.Function{
		Source: `(array-in-bounds-p (coerce "abcd" 'octets) 2)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(array-in-bounds-p (coerce "abcd" 'octets) 5)`,
		Expect: "nil",
	}).Test(t)
}

func TestArrayInBoundsPNotArray(t *testing.T) {
	(&sliptest.Function{
		Source:    `(array-in-bounds-p t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestArrayInBoundsPBadDims(t *testing.T) {
	(&sliptest.Function{
		Source:    `(array-in-bounds-p (make-array '(2 3)) 1)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(array-in-bounds-p (make-array '(2 3)) 1 2 3)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(array-in-bounds-p (make-array '(2 3)) 1 t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
