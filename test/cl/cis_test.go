// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestCisOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(cis 0)`,
		Expect: "#C(1 0)",
	}).Test(t)
	(&sliptest.Function{
		Source: `(cis 0.5)`,
		Expect: "/#C\\(0.877[0-9]+ 0.479[0-9]+\\)/",
	}).Test(t)
}

func TestCisNotNumber(t *testing.T) {
	(&sliptest.Function{
		Source:    `(cis t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
