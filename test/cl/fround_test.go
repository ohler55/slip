// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestFroundFixnum(t *testing.T) {
	key := slip.Symbol("*print-readably*")
	orig, _ := slip.GetVar(key)
	slip.SetVar(key, slip.True)
	defer slip.SetVar(key, orig)

	(&sliptest.Function{
		Source: `(fround 3)`,
		Expect: "3d+00, 0",
	}).Test(t)
	(&sliptest.Function{
		Source: `(fround 1 2)`,
		Expect: "0d+00, 1",
	}).Test(t)
	(&sliptest.Function{
		Source: `(fround 7 2)`,
		Expect: "4d+00, -1",
	}).Test(t)
	(&sliptest.Function{
		Source: `(fround -7 2)`,
		Expect: "-4d+00, 1",
	}).Test(t)
	(&sliptest.Function{
		Source: `(fround 5 2)`,
		Expect: "2d+00, 1",
	}).Test(t)
	(&sliptest.Function{
		Source: `(fround -5 2)`,
		Expect: "-2d+00, -1",
	}).Test(t)
	(&sliptest.Function{
		Source: `(fround 5 -2)`,
		Expect: "-2d+00, 1",
	}).Test(t)
	(&sliptest.Function{
		Source: `(fround -5 -2)`,
		Expect: "2d+00, -1",
	}).Test(t)
}

func TestFroundSingleFloat(t *testing.T) {
	key := slip.Symbol("*print-readably*")
	orig, _ := slip.GetVar(key)
	slip.SetVar(key, slip.True)
	defer slip.SetVar(key, orig)

	(&sliptest.Function{
		Source: `(fround 3.5s+0)`,
		Expect: "4s+00, -5s-01",
	}).Test(t)
	(&sliptest.Function{
		Source: `(fround 2.5s+0 2)`,
		Expect: "1s+00, 5s-01",
	}).Test(t)
	(&sliptest.Function{
		Source: `(fround 7.0s+0 2)`,
		Expect: "4s+00, -1s+00",
	}).Test(t)
	(&sliptest.Function{
		Source: `(fround -7.0s+0 2)`,
		Expect: "-4s+00, 1s+00",
	}).Test(t)
	(&sliptest.Function{
		Source: `(fround 5.0s+0 2)`,
		Expect: "2s+00, 1s+00",
	}).Test(t)
	(&sliptest.Function{
		Source: `(fround 5.5s+0 2)`,
		Expect: "3s+00, -5s-01",
	}).Test(t)
	(&sliptest.Function{
		Source: `(fround -5.5s+0 2)`,
		Expect: "-3s+00, 5s-01",
	}).Test(t)
	(&sliptest.Function{
		Source: `(fround 5.5s+0 -2)`,
		Expect: "-3s+00, -5s-01",
	}).Test(t)
	(&sliptest.Function{
		Source: `(fround -5.5s+0 -2)`,
		Expect: "3s+00, 5s-01",
	}).Test(t)
}

func TestFroundDoubleFloat(t *testing.T) {
	key := slip.Symbol("*print-readably*")
	orig, _ := slip.GetVar(key)
	slip.SetVar(key, slip.True)
	defer slip.SetVar(key, orig)

	(&sliptest.Function{
		Source: `(fround 3.5d+0)`,
		Expect: "4d+00, -5d-01",
	}).Test(t)
	(&sliptest.Function{
		Source: `(fround 2.5d+0 2)`,
		Expect: "1d+00, 5d-01",
	}).Test(t)
	(&sliptest.Function{
		Source: `(fround 7.0d+0 2)`,
		Expect: "4d+00, -1d+00",
	}).Test(t)
	(&sliptest.Function{
		Source: `(fround -7.0d+0 2)`,
		Expect: "-4d+00, 1d+00",
	}).Test(t)
	(&sliptest.Function{
		Source: `(fround 5.0d+0 2)`,
		Expect: "2d+00, 1d+00",
	}).Test(t)
	(&sliptest.Function{
		Source: `(fround 5.5d+0 2)`,
		Expect: "3d+00, -5d-01",
	}).Test(t)
	(&sliptest.Function{
		Source: `(fround -5.5d+0 2)`,
		Expect: "-3d+00, 5d-01",
	}).Test(t)
	(&sliptest.Function{
		Source: `(fround 5.5d+0 -2)`,
		Expect: "-3d+00, -5d-01",
	}).Test(t)
	(&sliptest.Function{
		Source: `(fround -5.5d+0 -2)`,
		Expect: "3d+00, 5d-01",
	}).Test(t)
}

func TestFroundLongFloat(t *testing.T) {
	key := slip.Symbol("*print-readably*")
	orig, _ := slip.GetVar(key)
	slip.SetVar(key, slip.True)
	defer slip.SetVar(key, orig)

	(&sliptest.Function{
		Source: `(fround 3.5L+0)`,
		Expect: "4L+00, -5L-01",
	}).Test(t)
	(&sliptest.Function{
		Source: `(fround 1.5L+0 2)`,
		Expect: "1L+00, -5L-01",
	}).Test(t)
	(&sliptest.Function{
		Source: `(fround 6.0L+0 2)`,
		Expect: "3L+00, 0L+00",
	}).Test(t)
	(&sliptest.Function{
		Source: `(fround 5.5L+0 2)`,
		Expect: "3L+00, -5L-01",
	}).Test(t)
	(&sliptest.Function{
		Source: `(fround -5.5L+0 2)`,
		Expect: "-3L+00, 5L-01",
	}).Test(t)
	(&sliptest.Function{
		Source: `(fround 5.5L+0 -2)`,
		Expect: "-3L+00, -5L-01",
	}).Test(t)
	(&sliptest.Function{
		Source: `(fround -5.5L+0 -2)`,
		Expect: "3L+00, 5L-01",
	}).Test(t)
	(&sliptest.Function{
		Source: `(fround 4.5L+0 2)`,
		Expect: "2L+00, 5L-01",
	}).Test(t)
	(&sliptest.Function{
		Source: `(fround 4.5L+0 -2)`,
		Expect: "-2L+00, 5L-01",
	}).Test(t)
	(&sliptest.Function{
		Source: `(fround -4.5L+0 2)`,
		Expect: "-2L+00, -5L-01",
	}).Test(t)
	(&sliptest.Function{
		Source: `(fround -4.5L+0 -2)`,
		Expect: "2L+00, -5L-01",
	}).Test(t)
}

func TestFroundBignum(t *testing.T) {
	key := slip.Symbol("*print-readably*")
	orig, _ := slip.GetVar(key)
	slip.SetVar(key, slip.True)
	defer slip.SetVar(key, orig)

	(&sliptest.Function{
		Source: `(fround 60000000000000000000 20000000000000000000)`,
		Expect: "3d+00, 0",
	}).Test(t)
	(&sliptest.Function{
		Source: `(fround 50000000000000000000 20000000000000000000)`,
		Expect: "2d+00, 10000000000000000000",
	}).Test(t)
	(&sliptest.Function{
		Source: `(fround 70000000000000000000 20000000000000000000)`,
		Expect: "4d+00, -10000000000000000000",
	}).Test(t)
	(&sliptest.Function{
		Source: `(fround 60000000000000000000 2)`,
		Expect: "3L+19, 0",
	}).Test(t)
	(&sliptest.Function{
		Source: `(fround 55000000000000000000 20000000000000000000)`,
		Expect: "3d+00, -5000000000000000000",
	}).Test(t)
	(&sliptest.Function{
		Source: `(fround -50000000000000000000 20000000000000000000)`,
		Expect: "-2d+00, -10000000000000000000",
	}).Test(t)
	(&sliptest.Function{
		Source: `(fround 50000000000000000000 -20000000000000000000)`,
		Expect: "-2d+00, 10000000000000000000",
	}).Test(t)
	(&sliptest.Function{
		Source: `(fround -50000000000000000000 -20000000000000000000)`,
		Expect: "2d+00, -10000000000000000000",
	}).Test(t)
}

func TestFroundRatio(t *testing.T) {
	key := slip.Symbol("*print-readably*")
	orig, _ := slip.GetVar(key)
	slip.SetVar(key, slip.True)
	defer slip.SetVar(key, orig)

	(&sliptest.Function{
		Source: `(fround 3/4)`,
		Expect: "1d+00, -1/4",
	}).Test(t)
	(&sliptest.Function{
		Source: `(fround 12/2 2)`,
		Expect: "3d+00, 0",
	}).Test(t)
	(&sliptest.Function{
		Source: `(fround -9/4)`,
		Expect: "-2d+00, -1/4",
	}).Test(t)
	(&sliptest.Function{
		Source: `(fround 3/4 1/2)`,
		Expect: "2d+00, -1/4",
	}).Test(t)
	(&sliptest.Function{
		Source: `(fround 3/4 -1/2)`,
		Expect: "-2d+00, -1/4",
	}).Test(t)
	(&sliptest.Function{
		Source: `(fround -3/4 1/2)`,
		Expect: "-2d+00, 1/4",
	}).Test(t)
	(&sliptest.Function{
		Source: `(fround -3/4 -1/2)`,
		Expect: "2d+00, 1/4",
	}).Test(t)
}

func TestFroundComplex(t *testing.T) {
	(&sliptest.Function{
		Source: `(fround #C(1 2) #C(1 2))`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(fround #C(1 0) 1)`,
		Panics: true,
	}).Test(t)
}

func TestFroundNotNumber(t *testing.T) {
	(&sliptest.Function{
		Source: `(fround t)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(fround t 1)`,
		Panics: true,
	}).Test(t)
}

func TestFroundNoArgs(t *testing.T) {
	(&sliptest.Function{
		Source: `(fround)`,
		Panics: true,
	}).Test(t)
}
