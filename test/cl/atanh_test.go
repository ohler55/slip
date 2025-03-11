// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestAtanhOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(atanh 0.5)`,
		Expect: "/0.54[0-9]+/",
	}).Test(t)
}

func TestAtanhNotNumber(t *testing.T) {
	(&sliptest.Function{
		Source:    `(atanh t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
