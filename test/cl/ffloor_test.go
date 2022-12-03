// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestFfloorFixnum(t *testing.T) {
	key := slip.Symbol("*print-readably*")
	orig, _ := slip.GetVar(key)
	slip.SetVar(key, slip.True)
	defer slip.SetVar(key, orig)

	(&sliptest.Function{
		Source: `(ffloor 3)`,
		Expect: "3d+00, 0",
	}).Test(t)
	(&sliptest.Function{
		Source: `(ffloor 1 2)`,
		Expect: "0d+00, 1",
	}).Test(t)
	(&sliptest.Function{
		Source: `(ffloor 5 2)`,
		Expect: "2d+00, 1",
	}).Test(t)
	(&sliptest.Function{
		Source: `(ffloor -5 2)`,
		Expect: "-3d+00, 1",
	}).Test(t)
	(&sliptest.Function{
		Source: `(ffloor -5 -2)`,
		Expect: "3d+00, 1",
	}).Test(t)
	(&sliptest.Function{
		Source: `(ffloor 5 -2)`,
		Expect: "-2d+00, 1",
	}).Test(t)
}

func TestFfloorSingleFloat(t *testing.T) {
	key := slip.Symbol("*print-readably*")
	orig, _ := slip.GetVar(key)
	slip.SetVar(key, slip.True)
	defer slip.SetVar(key, orig)

	(&sliptest.Function{
		Source: `(ffloor 3.5s+0)`,
		Expect: "3s+00, 5s-01",
	}).Test(t)
	(&sliptest.Function{
		Source: `(ffloor 5.5s+0 2)`,
		Expect: "2s+00, 1.5s+00",
	}).Test(t)
	(&sliptest.Function{
		Source: `(ffloor 1.5s+0 2)`,
		Expect: "0s+00, 1.5s+00",
	}).Test(t)
	(&sliptest.Function{
		Source: `(ffloor -5.5s+0 2)`,
		Expect: "-3s+00, 5s-01",
	}).Test(t)
	(&sliptest.Function{
		Source: `(ffloor -5.5s+0 -2)`,
		Expect: "2s+00, -1.5s+00",
	}).Test(t)
}

func TestFfloorDoubleFloat(t *testing.T) {
	key := slip.Symbol("*print-readably*")
	orig, _ := slip.GetVar(key)
	slip.SetVar(key, slip.True)
	defer slip.SetVar(key, orig)

	(&sliptest.Function{
		Source: `(ffloor 3.5d+0)`,
		Expect: "3d+00, 5d-01",
	}).Test(t)
	(&sliptest.Function{
		Source: `(ffloor 5.5d+0 2)`,
		Expect: "2d+00, 1.5d+00",
	}).Test(t)
	(&sliptest.Function{
		Source: `(ffloor 1.5d+0 2)`,
		Expect: "0d+00, 1.5d+00",
	}).Test(t)
	(&sliptest.Function{
		Source: `(ffloor -5.5d+0 2)`,
		Expect: "-3d+00, 5d-01",
	}).Test(t)
	(&sliptest.Function{
		Source: `(ffloor 5.5d+0 -2)`,
		Expect: "-3d+00, -5d-01",
	}).Test(t)
	(&sliptest.Function{
		Source: `(ffloor -5.5d+0 -2)`,
		Expect: "2d+00, -1.5d+00",
	}).Test(t)
}

func TestFfloorLongFloat(t *testing.T) {
	key := slip.Symbol("*print-readably*")
	orig, _ := slip.GetVar(key)
	slip.SetVar(key, slip.True)
	defer slip.SetVar(key, orig)

	(&sliptest.Function{
		Source: `(ffloor 3.5L+0)`,
		Expect: "3L+00, 5L-01",
	}).Test(t)
	(&sliptest.Function{
		Source: `(ffloor 5.5L+0 2)`,
		Expect: "2L+00, 1.5L+00",
	}).Test(t)
	(&sliptest.Function{
		Source: `(ffloor 4.0L+0 2)`,
		Expect: "2L+00, 0L+00",
	}).Test(t)
	(&sliptest.Function{
		Source: `(ffloor -5.5L+0 -2)`,
		Expect: "2L+00, -1.5L+00",
	}).Test(t)
	(&sliptest.Function{
		Source: `(ffloor 5.5L+0 -2)`,
		Expect: "-3L+00, -5L-01",
	}).Test(t)
	(&sliptest.Function{
		Source: `(ffloor 1.5L+0 2)`,
		Expect: "0L+00, 1.5L+00",
	}).Test(t)
	(&sliptest.Function{
		Source: `(ffloor -5.5L+0 2)`,
		Expect: "-3L+00, 5L-01",
	}).Test(t)
}

func TestFfloorBignum(t *testing.T) {
	key := slip.Symbol("*print-readably*")
	orig, _ := slip.GetVar(key)
	slip.SetVar(key, slip.True)
	defer slip.SetVar(key, orig)

	(&sliptest.Function{
		Source: `(ffloor 50000000000000000000 20000000000000000000)`,
		Expect: "2d+00, 10000000000000000000",
	}).Test(t)
	(&sliptest.Function{
		Source: `(ffloor -50000000000000000000 -20000000000000000000)`,
		Expect: "2d+00, -10000000000000000000",
	}).Test(t)
	(&sliptest.Function{
		Source: `(ffloor 60000000000000000000 20000000000000000000)`,
		Expect: "3d+00, 0",
	}).Test(t)
	(&sliptest.Function{
		Source: `(ffloor -50000000000000000000 20000000000000000000)`,
		Expect: "-3d+00, 10000000000000000000",
	}).Test(t)
	(&sliptest.Function{
		Source: `(ffloor 50000000000000000000 -20000000000000000000)`,
		Expect: "-3d+00, -10000000000000000000",
	}).Test(t)
	(&sliptest.Function{
		Source: `(ffloor 50000000000000000000 2)`,
		Expect: "2.5L+19, 0",
	}).Test(t)
}

func TestFfloorRatio(t *testing.T) {
	key := slip.Symbol("*print-readably*")
	orig, _ := slip.GetVar(key)
	slip.SetVar(key, slip.True)
	defer slip.SetVar(key, orig)

	(&sliptest.Function{
		Source: `(ffloor 3/4)`,
		Expect: "0d+00, 3/4",
	}).Test(t)
	(&sliptest.Function{
		Source: `(ffloor 12/2 2)`,
		Expect: "3d+00, 0",
	}).Test(t)
	(&sliptest.Function{
		Source: `(ffloor -9/4)`,
		Expect: "-3d+00, 3/4",
	}).Test(t)
	(&sliptest.Function{
		Source: `(ffloor 3/4 1/2)`,
		Expect: "1d+00, 1/4",
	}).Test(t)
	(&sliptest.Function{
		Source: `(ffloor 3/4 -1/2)`,
		Expect: "-2d+00, -1/4",
	}).Test(t)
	(&sliptest.Function{
		Source: `(ffloor -3/4 1/2)`,
		Expect: "-2d+00, 1/4",
	}).Test(t)
	(&sliptest.Function{
		Source: `(ffloor -3/4 -1/2)`,
		Expect: "1d+00, -1/4",
	}).Test(t)
}

func TestFfloorComplex(t *testing.T) {
	(&sliptest.Function{
		Source: `(ffloor #C(1 2) #C(1 2))`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(ffloor #C(1 0) 1)`,
		Panics: true,
	}).Test(t)
}

func TestFfloorNotNumber(t *testing.T) {
	(&sliptest.Function{
		Source: `(ffloor t)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(ffloor t 1)`,
		Panics: true,
	}).Test(t)
}

func TestFfloorNoArgs(t *testing.T) {
	(&sliptest.Function{
		Source: `(ffloor)`,
		Panics: true,
	}).Test(t)
}
