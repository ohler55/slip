// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestImagpartFixnum(t *testing.T) {
	(&sliptest.Function{
		Source: `(imagpart 5)`,
		Expect: "0",
	}).Test(t)
}

func TestImagpartSingleFloat(t *testing.T) {
	(&sliptest.Function{
		Source: `(imagpart 2.5s0)`,
		Expect: "0",
	}).Test(t)
}

func TestImagpartDoubleFloat(t *testing.T) {
	(&sliptest.Function{
		Source: `(imagpart 2.5)`,
		Expect: "0",
	}).Test(t)
}

func TestImagpartLongFloat(t *testing.T) {
	(&sliptest.Function{
		Source: `(imagpart 2.5L0)`,
		Expect: "0",
	}).Test(t)
}

func TestImagpartBignum(t *testing.T) {
	(&sliptest.Function{
		Source: `(imagpart 18446744073709551614)`,
		Expect: "0",
	}).Test(t)
}

func TestImagpartRatio(t *testing.T) {
	(&sliptest.Function{
		Source: `(imagpart 5/7)`,
		Expect: "0",
	}).Test(t)
}

func TestImagpartComplex(t *testing.T) {
	(&sliptest.Function{
		Source: `(imagpart #C(2.5 1.5))`,
		Expect: "1.5",
	}).Test(t)
}

func TestImagpartNotNumber(t *testing.T) {
	(&sliptest.Function{
		Source:    `(imagpart t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
