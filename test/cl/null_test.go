// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestNullFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(null 7)`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: `(null '(1))`,
		Expect: "nil",
	}).Test(t)
}

func TestNullTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(null '())`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(null nil)`,
		Expect: "t",
	}).Test(t)
}

func TestNullBadArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(null)`,
		Panics: true,
	}).Test(t)
}
