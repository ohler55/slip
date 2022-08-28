// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestExpFixnum(t *testing.T) {
	(&sliptest.Function{
		Source: `(exp 2)`,
		Expect: "7.38905609893065",
	}).Test(t)
	(&sliptest.Function{
		Source: `(exp -2)`,
		Expect: "0.1353352832366127",
	}).Test(t)
}

func TestExpSingleFloat(t *testing.T) {
	(&sliptest.Function{
		Source: `(exp 2.0s0)`,
		Expect: "7.38905609893065",
	}).Test(t)
	(&sliptest.Function{
		Source: `(exp -0.5s0)`,
		Expect: "0.6065306597126334",
	}).Test(t)
}

func TestExpDoubleFloat(t *testing.T) {
	(&sliptest.Function{
		Source: `(exp 2.0d0)`,
		Expect: "7.38905609893065",
	}).Test(t)
	(&sliptest.Function{
		Source: `(exp -0.5d0)`,
		Expect: "0.6065306597126334",
	}).Test(t)
}

func TestExpLongFloat(t *testing.T) {
	(&sliptest.Function{
		Source: `(exp 2.0L0)`,
		Expect: "7.38905609893065",
	}).Test(t)
	(&sliptest.Function{
		Source: `(exp -0.5L0)`,
		Expect: "0.6065306597126334",
	}).Test(t)
}

func TestExpRatio(t *testing.T) {
	(&sliptest.Function{
		Source: `(exp 1/2)`,
		Expect: "1.6487212707001282",
	}).Test(t)
	(&sliptest.Function{
		Source: `(exp -1/2)`,
		Expect: "0.6065306597126334",
	}).Test(t)
}

func TestExpBignum(t *testing.T) {
	(&sliptest.Function{
		Source: `(exp (- 100000000000000000010 100000000000000000000))`,
		Expect: "22026.465794806718",
	}).Test(t)
	(&sliptest.Function{
		Source: `(exp (- 100000000000000000000 100000000000000000010))`,
		Expect: "4.539992976248485e-05",
	}).Test(t)
}

func TestExpComplex(t *testing.T) {
	(&sliptest.Function{
		Source: `(exp #C(1 2))`,
		Expect: "#C(-1.1312043837568135 2.4717266720048183)",
	}).Test(t)
}

func TestExpBadArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(exp)`,
		Panics: true,
	}).Test(t)
}

func TestExpNotNumber(t *testing.T) {
	(&sliptest.Function{
		Source: `(exp t)`,
		Panics: true,
	}).Test(t)
}
