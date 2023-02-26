// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestUnlessTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(unless (zerop 1) 7)`,
		Expect: "7",
	}).Test(t)
	(&sliptest.Function{
		Source: `(unless nil 1 2 (+ 1 2))`,
		Expect: "3",
	}).Test(t)
	(&sliptest.Function{
		Source: `(unless nil)`,
		Expect: "nil",
	}).Test(t)
}

func TestUnlessFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(unless t 3)`,
		Expect: "nil",
	}).Test(t)
}

func TestUnlessArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(unless)`,
		Panics: true,
	}).Test(t)
}
