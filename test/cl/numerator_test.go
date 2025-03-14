// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestNumeratorFixnum(t *testing.T) {
	(&sliptest.Function{
		Source: `(numerator 5)`,
		Expect: "5",
	}).Test(t)
}

func TestNumeratorBignum(t *testing.T) {
	(&sliptest.Function{
		Source: `(numerator 18446744073709551614)`,
		Expect: "18446744073709551614",
	}).Test(t)
}

func TestNumeratorRatioFixnum(t *testing.T) {
	(&sliptest.Function{
		Source: `(numerator 5/7)`,
		Expect: "5",
	}).Test(t)
}

func TestNumeratorRatioBignum(t *testing.T) {
	(&sliptest.Function{
		Source: `(numerator -18446744073709551614/5)`,
		Expect: "-18446744073709551614",
	}).Test(t)
}

func TestNumeratorNotRational(t *testing.T) {
	(&sliptest.Function{
		Source:    `(numerator t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
