// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestNotFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(not 7)`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: `(not '(1))`,
		Expect: "nil",
	}).Test(t)
}

func TestNotTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(not '())`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(not nil)`,
		Expect: "t",
	}).Test(t)
}

func TestNotBadArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(not)`,
		Panics: true,
	}).Test(t)
}
