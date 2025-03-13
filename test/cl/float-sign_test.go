// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestFloatSignSingle(t *testing.T) {
	(&sliptest.Function{
		Source:   `(float-sign 5s0)`,
		Readably: true,
		Expect:   "1s+00",
	}).Test(t)
	(&sliptest.Function{
		Source:   `(float-sign -5s0)`,
		Readably: true,
		Expect:   "-1s+00",
	}).Test(t)
	(&sliptest.Function{
		Source:   `(float-sign 5s0 3.0)`,
		Readably: true,
		Expect:   "3s+00",
	}).Test(t)
	(&sliptest.Function{
		Source:   `(float-sign -5s0 3.0)`,
		Readably: true,
		Expect:   "-3s+00",
	}).Test(t)
}

func TestFloatSignDouble(t *testing.T) {
	(&sliptest.Function{
		Source:   `(float-sign 5d0)`,
		Readably: true,
		Expect:   "1d+00",
	}).Test(t)
	(&sliptest.Function{
		Source:   `(float-sign -5d0)`,
		Readably: true,
		Expect:   "-1d+00",
	}).Test(t)
	(&sliptest.Function{
		Source:   `(float-sign 5d0 -3.0)`,
		Readably: true,
		Expect:   "3d+00",
	}).Test(t)
	(&sliptest.Function{
		Source:   `(float-sign -5d0 3.0)`,
		Readably: true,
		Expect:   "-3d+00",
	}).Test(t)
}

func TestFloatSignLong(t *testing.T) {
	(&sliptest.Function{
		Source:   `(float-sign 5L0)`,
		Readably: true,
		Expect:   "1L+00",
	}).Test(t)
	(&sliptest.Function{
		Source:   `(float-sign -5L0)`,
		Readably: true,
		Expect:   "-1L+00",
	}).Test(t)
	(&sliptest.Function{
		Source:   `(float-sign 5L0 3.0)`,
		Readably: true,
		Expect:   "3L+00",
	}).Test(t)
	(&sliptest.Function{
		Source:   `(float-sign -5L0 3.0)`,
		Readably: true,
		Expect:   "-3L+00",
	}).Test(t)
}

func TestFloatSignNotFloat(t *testing.T) {
	(&sliptest.Function{
		Source:    `(float-sign t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(float-sign 3.0 t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
