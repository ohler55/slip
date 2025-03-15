// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestCoshOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(cosh 0)`,
		Expect: "1",
	}).Test(t)
	(&sliptest.Function{
		Source: `(cosh 0.5)`,
		Expect: "/1.1[0-9]+/",
	}).Test(t)
}

func TestCoshNotNumber(t *testing.T) {
	(&sliptest.Function{
		Source:    `(cosh t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
