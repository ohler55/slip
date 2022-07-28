// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestSqrtFixnum(t *testing.T) {
	(&sliptest.Function{
		Source: `(sqrt 9)`,
		Expect: "3",
	}).Test(t)
	(&sliptest.Function{
		Source: `(sqrt -9)`,
		Expect: "#C(0 3)",
	}).Test(t)
}

func TestSqrtSingleFloat(t *testing.T) {
	(&sliptest.Function{
		Source: `(sqrt 9.0s0)`,
		Expect: "3",
	}).Test(t)
	(&sliptest.Function{
		Source: `(sqrt -9.0s0)`,
		Expect: "#C(0 3)",
	}).Test(t)
}

func TestSqrtDoubleFloat(t *testing.T) {
	(&sliptest.Function{
		Source: `(sqrt 9.0d0)`,
		Expect: "3",
	}).Test(t)
	(&sliptest.Function{
		Source: `(sqrt -9.0d0)`,
		Expect: "#C(0 3)",
	}).Test(t)
}

func TestSqrtLongFloat(t *testing.T) {
	(&sliptest.Function{
		Source: `(sqrt 9.0l0)`,
		Expect: "3",
	}).Test(t)
	(&sliptest.Function{
		Source: `(sqrt -9.0l0)`,
		Expect: "#C(0 3)",
	}).Test(t)
}

func TestSqrtRatioFloat(t *testing.T) {
	(&sliptest.Function{
		Source: `(sqrt 9/4)`,
		Expect: "1.5",
	}).Test(t)
	(&sliptest.Function{
		Source: `(sqrt -9/4)`,
		Expect: "#C(0 1.5)",
	}).Test(t)
}

func TestSqrtRatioBignum(t *testing.T) {
	(&sliptest.Function{
		Source: `(sqrt 100000000000000000000)`,
		Expect: "1e+10",
	}).Test(t)
	(&sliptest.Function{
		Source: `(sqrt -100000000000000000000)`,
		Expect: "#C(0 1e+10)",
	}).Test(t)
}

func TestSqrtBadArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(sqrt)`,
		Panics: true,
	}).Test(t)
}

func TestSqrtNotNumber(t *testing.T) {
	(&sliptest.Function{
		Source: `(sqrt t)`,
		Panics: true,
	}).Test(t)
}
