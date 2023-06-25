// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestRationalFixnum(t *testing.T) {
	(&sliptest.Function{
		Source: `(rational 3)`,
		Expect: "3",
	}).Test(t)
}

func TestRationalSingleFloat(t *testing.T) {
	(&sliptest.Function{
		Source: `(rational 3.0s+0)`,
		Expect: "3",
	}).Test(t)
	(&sliptest.Function{
		Source: `(rational 0.1s+0)`,
		Expect: "13421773/134217728",
	}).Test(t)
}

func TestRationalDoubleFloat(t *testing.T) {
	(&sliptest.Function{
		Source: `(rational 3.0d+0)`,
		Expect: "3",
	}).Test(t)
	(&sliptest.Function{
		Source: `(rational 0.1d+0)`,
		Expect: "3602879701896397/36028797018963968",
	}).Test(t)
}

func TestRationalLongFloat(t *testing.T) {
	(&sliptest.Function{
		Source: `(rational 3.0L+0)`,
		Expect: "3",
	}).Test(t)
	(&sliptest.Function{
		Source: `(rational 3.000000000000000000000L+20)`,
		Expect: "300000000000000000000",
	}).Test(t)
	(&sliptest.Function{
		Source: `(rational 0.1L+0)`,
		Expect: "205/2048",
	}).Test(t)
}

func TestRationalBignum(t *testing.T) {
	(&sliptest.Function{
		Source: `(rational 30000000000000000000)`,
		Expect: "30000000000000000000",
	}).Test(t)
}

func TestRationalRatio(t *testing.T) {
	(&sliptest.Function{
		Source: `(rational 3/4)`,
		Expect: "3/4",
	}).Test(t)
}

func TestRationalComplex(t *testing.T) {
	(&sliptest.Function{
		Source: `(rational 1 #C(1 2))`,
		Panics: true,
	}).Test(t)
}

func TestRationalNotNumber(t *testing.T) {
	(&sliptest.Function{
		Source: `(rational t)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(rational t 1)`,
		Panics: true,
	}).Test(t)
}

func TestRationalNoArgs(t *testing.T) {
	(&sliptest.Function{
		Source: `(rational)`,
		Panics: true,
	}).Test(t)
}
