// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestComplexpTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(complexp #C(1 2))`,
		Expect: "t",
	}).Test(t)
}

func TestComplexpFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(complexp 3)`,
		Expect: "nil",
	}).Test(t)
}
