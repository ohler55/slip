// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestFloatDigitsSingle(t *testing.T) {
	(&sliptest.Function{
		Source: `(float-digits 5s0)`,
		Expect: "24",
	}).Test(t)
}

func TestFloatDigitsDouble(t *testing.T) {
	(&sliptest.Function{
		Source: `(float-digits 5d0)`,
		Expect: "53",
	}).Test(t)
}

func TestFloatDigitsLong(t *testing.T) {
	(&sliptest.Function{
		Source: `(float-digits 5.1234L0)`,
		Expect: "19",
	}).Test(t)
}

func TestFloatDigitsNotRational(t *testing.T) {
	(&sliptest.Function{
		Source:    `(float-digits t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
