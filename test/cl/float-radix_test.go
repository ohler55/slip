// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestFloatRadixOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(float-radix 5s0)`,
		Expect: "2",
	}).Test(t)
}

func TestFloatRadixNotRational(t *testing.T) {
	(&sliptest.Function{
		Source:    `(float-radix t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
