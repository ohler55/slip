// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestNumberpTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(numberp 4)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(numberp 4.1)`,
		Expect: "t",
	}).Test(t)
}

func TestNumberpFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(numberp t)`,
		Expect: "nil",
	}).Test(t)
}

func TestNumberpBadArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(numberp)`,
		Panics: true,
	}).Test(t)
}
