// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestConspFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(consp 7)`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: `(consp '())`,
		Expect: "nil",
	}).Test(t)
}

func TestConspTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(consp '(1 2))`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(consp '(1 . 2))`,
		Expect: "t",
	}).Test(t)
}

func TestConspBadArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(consp)`,
		Panics: true,
	}).Test(t)
}
