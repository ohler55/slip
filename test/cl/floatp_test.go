// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestFloattpTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(floatp 4.2)`,
		Expect: "t",
	}).Test(t)
}

func TestFloattpFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(floatp 4)`,
		Expect: "nil",
	}).Test(t)
}

func TestFloattpBadArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(floatp)`,
		Panics: true,
	}).Test(t)
}
