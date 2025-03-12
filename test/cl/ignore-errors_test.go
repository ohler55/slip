// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestIgnoreErrorsCatch(t *testing.T) {
	(&sliptest.Function{
		Source: `(ignore-errors (/ 1 0))`,
		Expect: "/^nil, #<DIVISION-BY-ZERO [0-9a-f]+>$/",
	}).Test(t)
}

func TestIgnoreErrorsOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(ignore-errors (+ 1 2))`,
		Expect: "3",
	}).Test(t)
}
