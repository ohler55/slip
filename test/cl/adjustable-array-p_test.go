// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestAdjustableArrayPVector(t *testing.T) {
	(&sliptest.Function{
		Source: `(adjustable-array-p (make-array 4 :adjustable t))`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(adjustable-array-p (make-array 4 :adjustable nil))`,
		Expect: "nil",
	}).Test(t)
}

func TestAdjustableArrayPArray(t *testing.T) {
	(&sliptest.Function{
		Source: `(adjustable-array-p (make-array '(2 3) :adjustable t))`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(adjustable-array-p (make-array '(2 3) :adjustable nil))`,
		Expect: "nil",
	}).Test(t)
}

func TestAdjustableArrayPOctets(t *testing.T) {
	(&sliptest.Function{
		Source: `(adjustable-array-p (coerce #(1 2 3) 'octets))`,
		Expect: "nil",
	}).Test(t)
}

func TestAdjustableArrayPNotArray(t *testing.T) {
	(&sliptest.Function{
		Source:    `(adjustable-array-p t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
