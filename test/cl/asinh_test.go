// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestAsinhOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(asinh 1.5)`,
		Expect: "/1.19[0-9]+/",
	}).Test(t)
}

func TestAsinhNotNumber(t *testing.T) {
	(&sliptest.Function{
		Source:    `(asinh t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
