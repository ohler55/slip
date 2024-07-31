// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestArrayTotalSizeVector(t *testing.T) {
	(&sliptest.Function{
		Source: `(array-total-size (make-array 4))`,
		Expect: "4",
	}).Test(t)
}

func TestArrayTotalSizeArray(t *testing.T) {
	(&sliptest.Function{
		Source: `(array-total-size (make-array '(2 3)))`,
		Expect: "6",
	}).Test(t)
}

func TestArrayTotalSizeOctets(t *testing.T) {
	(&sliptest.Function{
		Source: `(array-total-size (coerce "abcd" 'octets))`,
		Expect: "4",
	}).Test(t)
}

func TestArrayTotalSizeNotArray(t *testing.T) {
	(&sliptest.Function{
		Source:    `(array-total-size t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
