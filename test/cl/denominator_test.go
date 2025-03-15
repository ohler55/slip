// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestDenominatorFixnum(t *testing.T) {
	(&sliptest.Function{
		Source: `(denominator 5)`,
		Expect: "1",
	}).Test(t)
}

func TestDenominatorBignum(t *testing.T) {
	(&sliptest.Function{
		Source: `(denominator 18446744073709551614)`,
		Expect: "1",
	}).Test(t)
}

func TestDenominatorRatioFixnum(t *testing.T) {
	(&sliptest.Function{
		Source: `(denominator 5/7)`,
		Expect: "7",
	}).Test(t)
}

func TestDenominatorRatioBignum(t *testing.T) {
	(&sliptest.Function{
		Source: `(denominator 5/18446744073709551614)`,
		Expect: "18446744073709551614",
	}).Test(t)
}

func TestDenominatorNotRational(t *testing.T) {
	(&sliptest.Function{
		Source:    `(denominator t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
