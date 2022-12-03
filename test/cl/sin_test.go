// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestSinFixnum(t *testing.T) {
	(&sliptest.Function{
		Source: `(sin 0)`,
		Expect: "0",
	}).Test(t)
}

func TestSinSingleFloat(t *testing.T) {
	(&sliptest.Function{
		Source: `(sin 0.0s0)`,
		Expect: "0",
	}).Test(t)
}

func TestSinDoubleFloat(t *testing.T) {
	(&sliptest.Function{
		Source: `(sin 0.0d0)`,
		Expect: "0",
	}).Test(t)
}

func TestSinLongFloat(t *testing.T) {
	(&sliptest.Function{
		Source: `(sin 0.0l0)`,
		Expect: "0",
	}).Test(t)
}

func TestSinRatio(t *testing.T) {
	(&sliptest.Function{
		Source: `(sin 0/2)`,
		Expect: "0",
	}).Test(t)
}

func TestSinBadArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(sin)`,
		Panics: true,
	}).Test(t)
}

func TestSinNotNumber(t *testing.T) {
	(&sliptest.Function{
		Source: `(sin t)`,
		Panics: true,
	}).Test(t)
}
