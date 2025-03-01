// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestMultipleValueSetqBasic(t *testing.T) {
	(&sliptest.Function{
		Source: `(let (x y) (multiple-value-setq (x y) (floor 130 11)) (list x y))`,
		Expect: "(11 9)",
	}).Test(t)
}

func TestMultipleValueSetqTooFew(t *testing.T) {
	(&sliptest.Function{
		Source: `(let (x) (multiple-value-setq (x) (floor 130 11)) x)`,
		Expect: "11",
	}).Test(t)
}

func TestMultipleValueSetqTooMany(t *testing.T) {
	(&sliptest.Function{
		Source: `(let (x y z) (multiple-value-setq (x y z) (floor 130 11)) (list x y z))`,
		Expect: "(11 9 nil)",
	}).Test(t)
}

func TestMultipleValueSetqSingle(t *testing.T) {
	(&sliptest.Function{
		Source: `(let (x y) (multiple-value-setq (x y) (+ 1 2)) (list x y))`,
		Expect: "(3 nil)",
	}).Test(t)
}

func TestMultipleValueSetqBadVars(t *testing.T) {
	(&sliptest.Function{
		Source:    `(multiple-value-setq (t) (+ 1 2))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(multiple-value-setq t (+ 1 2))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
