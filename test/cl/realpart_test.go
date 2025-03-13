// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestRealpartFixnum(t *testing.T) {
	(&sliptest.Function{
		Source: `(realpart 5)`,
		Expect: "5",
	}).Test(t)
}

func TestRealpartFloat(t *testing.T) {
	(&sliptest.Function{
		Source: `(realpart 2.5)`,
		Expect: "2.5",
	}).Test(t)
}

func TestRealpartBignum(t *testing.T) {
	(&sliptest.Function{
		Source: `(realpart 18446744073709551614)`,
		Expect: "18446744073709551614",
	}).Test(t)
}

func TestRealpartRatio(t *testing.T) {
	(&sliptest.Function{
		Source: `(realpart 5/7)`,
		Expect: "5/7",
	}).Test(t)
}

func TestRealpartComplex(t *testing.T) {
	(&sliptest.Function{
		Source: `(realpart #C(2.5 1.5))`,
		Expect: "2.5",
	}).Test(t)
}

func TestRealpartNotNumber(t *testing.T) {
	(&sliptest.Function{
		Source:    `(realpart t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
