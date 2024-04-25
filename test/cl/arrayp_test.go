// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestArraypVector(t *testing.T) {
	(&sliptest.Function{
		Source: `(arrayp (make-array 4))`,
		Expect: "t",
	}).Test(t)
}

func TestArraypArray(t *testing.T) {
	(&sliptest.Function{
		Source: `(arrayp (make-array '(2 3)))`,
		Expect: "t",
	}).Test(t)
}

func TestArraypNotArray(t *testing.T) {
	(&sliptest.Function{
		Source: `(arrayp t)`,
		Expect: "nil",
	}).Test(t)
}
