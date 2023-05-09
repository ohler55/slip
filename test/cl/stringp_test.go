// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestStringpTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(stringp "abc")`,
		Expect: "t",
	}).Test(t)
}

func TestStringpFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(stringp t)`,
		Expect: "nil",
	}).Test(t)
}

func TestStringpBadArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(stringp)`,
		Panics: true,
	}).Test(t)
}
