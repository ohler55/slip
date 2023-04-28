// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestWhenTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(when (zerop 0) 7)`,
		Expect: "7",
	}).Test(t)
	(&sliptest.Function{
		Source: `(when t 1 2 (+ 1 2))`,
		Expect: "3",
	}).Test(t)
	(&sliptest.Function{
		Source: `(when t)`,
		Expect: "nil",
	}).Test(t)
}

func TestWhenFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(when '() 3)`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: `(when nil 3)`,
		Expect: "nil",
	}).Test(t)
}

func TestWhenArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(when)`,
		Panics: true,
	}).Test(t)
}
