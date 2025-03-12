// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestDecodeFloatOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(decode-float -0.125)`,
		Expect: "0.5, -2, -1",
	}).Test(t)
}

func TestDecodeFloatNotNumber(t *testing.T) {
	(&sliptest.Function{
		Source:    `(decode-float t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
