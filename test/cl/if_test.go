// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestIfTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(if (zerop 0) 3 4)`,
		Expect: "3",
	}).Test(t)
	(&sliptest.Function{
		Source: `(if t (+ 1 2) (+ 2 3))`,
		Expect: "3",
	}).Test(t)
}

func TestIfFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(if (zerop 1) 3 4)`,
		Expect: "4",
	}).Test(t)
	(&sliptest.Function{
		Source: `(if nil 3)`,
		Expect: "nil",
	}).Test(t)
}

func TestIfArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(if t)`,
		Panics: true,
	}).Test(t)
}
