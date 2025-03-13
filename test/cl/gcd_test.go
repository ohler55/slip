// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestGcdZero(t *testing.T) {
	(&sliptest.Function{
		Source: `(gcd)`,
		Expect: "0",
	}).Test(t)
}

func TestGcdOne(t *testing.T) {
	(&sliptest.Function{
		Source: `(gcd 8)`,
		Expect: "8",
	}).Test(t)
}

func TestGcdTwo(t *testing.T) {
	(&sliptest.Function{
		Source: `(gcd 42 70)`,
		Expect: "14",
	}).Test(t)
}

func TestGcdMulti(t *testing.T) {
	(&sliptest.Function{
		Source: `(gcd 42 70 8)`,
		Expect: "2",
	}).Test(t)
}

func TestGcdNotFixnum(t *testing.T) {
	(&sliptest.Function{
		Source:    `(gcd t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
