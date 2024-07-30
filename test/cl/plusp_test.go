// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestPlustpTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(plusp 5)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(plusp 5.5)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(plusp (coerce 5 'octet))`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(plusp 5.5s-1)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(plusp 5.5l-1)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(plusp 5/4)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(plusp 12297829382473034410)`,
		Expect: "t",
	}).Test(t)
}

func TestPlustpFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(plusp -5)`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: `(plusp 0)`,
		Expect: "nil",
	}).Test(t)
}

func TestPlustpBadArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(plusp)`,
		Panics: true,
	}).Test(t)
}

func TestPlustpNotNumber(t *testing.T) {
	(&sliptest.Function{
		Source: `(plusp t)`,
		Panics: true,
	}).Test(t)
}
