// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestTypeOf(t *testing.T) {
	(&sliptest.Function{
		Source: `(type-of 7)`,
		Expect: "fixnum",
	}).Test(t)
	(&sliptest.Function{
		Source: `(type-of '())`,
		Expect: "null",
	}).Test(t)
	(&sliptest.Function{
		Source: `(type-of nil)`,
		Expect: "null",
	}).Test(t)
}

func TestTypeOfBadArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(type-of)`,
		Panics: true,
	}).Test(t)
}
