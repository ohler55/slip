// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestTanhOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(tanh 0)`,
		Expect: "0",
	}).Test(t)
	(&sliptest.Function{
		Source: `(tanh 0.5)`,
		Expect: "/0.46[0-9]+/",
	}).Test(t)
}

func TestTanhNotNumber(t *testing.T) {
	(&sliptest.Function{
		Source:    `(tanh t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
