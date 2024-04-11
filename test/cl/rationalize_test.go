// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestRationalizeFixnum(t *testing.T) {
	(&sliptest.Function{
		Source: `(rationalize 3)`,
		Expect: "3",
	}).Test(t)
}

func TestRationalizeSingleFloat(t *testing.T) {
	(&sliptest.Function{
		Source: `(rationalize 3.0s+0)`,
		Expect: "3",
	}).Test(t)
	(&sliptest.Function{
		Source: `(rationalize 0.5s+0)`,
		Expect: "1/2",
	}).Test(t)
}

func TestRationalizeDoubleFloat(t *testing.T) {
	(&sliptest.Function{
		Source: `(rationalize 3.0d+0)`,
		Expect: "3",
	}).Test(t)
	(&sliptest.Function{
		Source: `(rationalize 0.1d+0)`,
		Expect: "1/10",
	}).Test(t)
	(&sliptest.Function{
		Source: `(rationalize -0.75d+0)`,
		Expect: "-3/4",
	}).Test(t)
	(&sliptest.Function{
		Source: `(rationalize 0.5d+2)`,
		Expect: "50",
	}).Test(t)
	(&sliptest.Function{
		Source: `(rationalize -0.12345e-20)`,
		Expect: "-8895509983982205/72057594037927936",
	}).Test(t)
}

func TestRationalizeLongFloat(t *testing.T) {
	(&sliptest.Function{
		Source: `(rationalize 3.0L+0)`,
		Expect: "3",
	}).Test(t)
	(&sliptest.Function{
		Source: `(rationalize 3.000000000000000000000L+20)`,
		Expect: "300000000000000000000",
	}).Test(t)
	(&sliptest.Function{
		Source: `(rationalize 0.1L+0)`,
		Expect: "209715/2097152",
	}).Test(t)
}

func TestRationalizeBignum(t *testing.T) {
	(&sliptest.Function{
		Source: `(rationalize 30000000000000000000)`,
		Expect: "30000000000000000000",
	}).Test(t)
}

func TestRationalizeRatio(t *testing.T) {
	(&sliptest.Function{
		Source: `(rationalize 3/4)`,
		Expect: "3/4",
	}).Test(t)
}

func TestRationalizeComplex(t *testing.T) {
	(&sliptest.Function{
		Source: `(rationalize 1 #C(1 2))`,
		Panics: true,
	}).Test(t)
}

func TestRationalizeNotNumber(t *testing.T) {
	(&sliptest.Function{
		Source: `(rationalize t)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(rationalize t 1)`,
		Panics: true,
	}).Test(t)
}

func TestRationalizeNoArgs(t *testing.T) {
	(&sliptest.Function{
		Source: `(rationalize)`,
		Panics: true,
	}).Test(t)
}
