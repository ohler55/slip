// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestAbsFixnum(t *testing.T) {
	(&sliptest.Function{
		Source: `(abs -3)`,
		Expect: "3",
	}).Test(t)
}

func TestAbsSingleFloat(t *testing.T) {
	(&sliptest.Function{
		Source: `(abs -3.1s0)`,
		Expect: "3.1",
	}).Test(t)
}

func TestAbsDoubleFloat(t *testing.T) {
	(&sliptest.Function{
		Source: `(abs -3.1d0)`,
		Expect: "3.1",
	}).Test(t)
}

func TestAbsLongFloat(t *testing.T) {
	(&sliptest.Function{
		Source: `(abs -3.1l0)`,
		Expect: "3.1",
	}).Test(t)
}

func TestAbsRatio(t *testing.T) {
	(&sliptest.Function{
		Source: `(abs -3/2)`,
		Expect: "3/2",
	}).Test(t)
}

func TestAbsBignum(t *testing.T) {
	(&sliptest.Function{
		Source: `(abs -12297829382473034410)`,
		Expect: "12297829382473034410",
	}).Test(t)
}

func TestAbsComplex(t *testing.T) {
	(&sliptest.Function{
		Source: `(abs #c(-1 2))`,
		Expect: "#C(1 2)",
	}).Test(t)
	(&sliptest.Function{
		Source: `(abs #c(1 -2))`,
		Expect: "#C(1 2)",
	}).Test(t)
}

func TestAbsBadArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(abs)`,
		Panics: true,
	}).Test(t)
}

func TestAbsNotNumber(t *testing.T) {
	(&sliptest.Function{
		Source: `(abs t)`,
		Panics: true,
	}).Test(t)
}
