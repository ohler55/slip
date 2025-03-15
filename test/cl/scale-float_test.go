// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestScaleFloatOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(scale-float 1.5 2)`,
		Expect: "6",
	}).Test(t)
}

func TestScaleFloatNotNumber(t *testing.T) {
	(&sliptest.Function{
		Source:    `(scale-float t 1)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(scale-float 1.5 t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
