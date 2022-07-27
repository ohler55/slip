// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestAsinFixnum(t *testing.T) {
	(&sliptest.Function{
		Source: `(asin -1)`,
		Expect: "-1.5707963267948966",
	}).Test(t)
}

func TestAsinSingleFloat(t *testing.T) {
	(&sliptest.Function{
		Source: `(asin 1.0s0)`,
		Expect: "1.5707963267948966",
	}).Test(t)
}

func TestAsinDoubleFloat(t *testing.T) {
	(&sliptest.Function{
		Source: `(asin 1.0d0)`,
		Expect: "1.5707963267948966",
	}).Test(t)
}

func TestAsinLongFloat(t *testing.T) {
	(&sliptest.Function{
		Source: `(asin 1.0l0)`,
		Expect: "1.5707963267948966",
	}).Test(t)
}

func TestAsinRatio(t *testing.T) {
	(&sliptest.Function{
		Source: `(asin 1/2)`,
		Expect: "0.5235987755982989",
	}).Test(t)
}

func TestAsinBadArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(asin)`,
		Panics: true,
	}).Test(t)
}

func TestAsinNotNumber(t *testing.T) {
	(&sliptest.Function{
		Source: `(asin t)`,
		Panics: true,
	}).Test(t)
}
