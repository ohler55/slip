// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestArrayRankVector(t *testing.T) {
	(&sliptest.Function{
		Source: `(array-rank (make-array 4))`,
		Expect: "1",
	}).Test(t)
}

func TestArrayRankArray(t *testing.T) {
	(&sliptest.Function{
		Source: `(array-rank (make-array '(2 3)))`,
		Expect: "2",
	}).Test(t)
}

func TestArrayRankNotArray(t *testing.T) {
	(&sliptest.Function{
		Source:    `(array-rank t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
