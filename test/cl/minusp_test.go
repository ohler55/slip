// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestMinustpTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(minusp -5)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(minusp -5.5)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(minusp -5.5s-1)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(minusp -5.5l-1)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(minusp -5/4)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(minusp -12297829382473034410)`,
		Expect: "t",
	}).Test(t)
}

func TestMinustpFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(minusp 5)`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: `(minusp 0)`,
		Expect: "nil",
	}).Test(t)
}

func TestMinustpBadArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(minusp)`,
		Panics: true,
	}).Test(t)
}

func TestMinustpNotNumber(t *testing.T) {
	(&sliptest.Function{
		Source: `(minusp t)`,
		Panics: true,
	}).Test(t)
}
