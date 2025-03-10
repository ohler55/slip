// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestSinhOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(sinh 0)`,
		Expect: "0",
	}).Test(t)
	(&sliptest.Function{
		Source: `(sinh 0.5)`,
		Expect: "/0.5[0-9]+/",
	}).Test(t)
}

func TestSinhNotNumber(t *testing.T) {
	(&sliptest.Function{
		Source:    `(sinh t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
