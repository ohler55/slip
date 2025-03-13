// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestFloatPrecisionSingle(t *testing.T) {
	(&sliptest.Function{
		Source: `(float-precision 5s0)`,
		Expect: "24",
	}).Test(t)
}

func TestFloatPrecisionDouble(t *testing.T) {
	(&sliptest.Function{
		Source: `(float-precision 5d0)`,
		Expect: "53",
	}).Test(t)
}

func TestFloatPrecisionLong(t *testing.T) {
	(&sliptest.Function{
		Source: `(float-precision 5.1234L0)`,
		Expect: "19",
	}).Test(t)
}

func TestFloatPrecisionNotRational(t *testing.T) {
	(&sliptest.Function{
		Source:    `(float-precision t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
