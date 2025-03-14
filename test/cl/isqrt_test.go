// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestIsqrtFixnum(t *testing.T) {
	(&sliptest.Function{
		Source: `(isqrt 21)`,
		Expect: "4",
	}).Test(t)
}

func TestIsqrtRatio(t *testing.T) {
	(&sliptest.Function{
		Source: `(isqrt 25/9)`,
		Expect: "1",
	}).Test(t)
}

func TestIsqrtFloat(t *testing.T) {
	(&sliptest.Function{
		Source: `(isqrt 21.5)`,
		Expect: "4",
	}).Test(t)
}

func TestIsqrtLongFloat(t *testing.T) {
	(&sliptest.Function{
		Source: `(isqrt 21.5L0)`,
		Expect: "4",
	}).Test(t)
}

func TestIsqrtBignum(t *testing.T) {
	(&sliptest.Function{
		Source: `(isqrt 900000000000000000000)`,
		Expect: "30000000000",
	}).Test(t)
}

func TestIsqrtNegative(t *testing.T) {
	(&sliptest.Function{
		Source:    `(isqrt -9)`,
		PanicType: slip.ArithmeticErrorSymbol,
	}).Test(t)
}

func TestIsqrtNotReal(t *testing.T) {
	(&sliptest.Function{
		Source:    `(isqrt t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
