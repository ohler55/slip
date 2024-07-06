// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestRationalpTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(rationalp 4)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(rationalp 4/3)`,
		Expect: "t",
	}).Test(t)
}

func TestRationalpFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(rationalp t)`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: `(rationalp 4.1)`,
		Expect: "nil",
	}).Test(t)
}
