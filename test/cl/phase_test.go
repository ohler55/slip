// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestPhaseFixnum(t *testing.T) {
	(&sliptest.Function{
		Source:   `(phase 0)`,
		Readably: true,
		Expect:   "0d+00",
	}).Test(t)
	(&sliptest.Function{
		Source:   `(phase 1)`,
		Readably: true,
		Expect:   "0d+00",
	}).Test(t)
}

func TestPhaseFloat(t *testing.T) {
	(&sliptest.Function{
		Source: `(phase -0.1)`,
		Expect: "/^3.14159[0-9]+$/",
	}).Test(t)
}

func TestPhaseComplex(t *testing.T) {
	(&sliptest.Function{
		Source: `(phase #C(0 1))`,
		Expect: "/^1.57079[0-9]+$/",
	}).Test(t)
}

func TestPhaseNotNumber(t *testing.T) {
	(&sliptest.Function{
		Source:    `(phase t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
