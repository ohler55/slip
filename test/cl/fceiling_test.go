// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestFceilingFixnum(t *testing.T) {
	key := slip.Symbol("*print-readably*")
	orig, _ := slip.GetVar(key)
	slip.SetVar(key, slip.True)
	defer slip.SetVar(key, orig)

	(&sliptest.Function{
		Source: `(fceiling 3)`,
		Expect: "3d+00, 0",
	}).Test(t)
	(&sliptest.Function{
		Source: `(fceiling 1 2)`,
		Expect: "1d+00, -1",
	}).Test(t)
	(&sliptest.Function{
		Source: `(fceiling 5 2)`,
		Expect: "3d+00, -1",
	}).Test(t)
	(&sliptest.Function{
		Source: `(fceiling -5 2)`,
		Expect: "-2d+00, -1",
	}).Test(t)
}

func TestFceilingSingleFloat(t *testing.T) {
	key := slip.Symbol("*print-readably*")
	orig, _ := slip.GetVar(key)
	slip.SetVar(key, slip.True)
	defer slip.SetVar(key, orig)

	(&sliptest.Function{
		Source: `(fceiling 3.5s+0)`,
		Expect: "4s+00, -5s-01",
	}).Test(t)
	(&sliptest.Function{
		Source: `(fceiling 5.5s+0 2)`,
		Expect: "3s+00, -5s-01",
	}).Test(t)
	(&sliptest.Function{
		Source: `(fceiling 1.5s+0 2)`,
		Expect: "1s+00, -5s-01",
	}).Test(t)
	(&sliptest.Function{
		Source: `(fceiling -5.5s+0 2)`,
		Expect: "-2s+00, -1.5s+00",
	}).Test(t)
}

func TestFceilingDoubleFloat(t *testing.T) {
	key := slip.Symbol("*print-readably*")
	orig, _ := slip.GetVar(key)
	slip.SetVar(key, slip.True)
	defer slip.SetVar(key, orig)

	(&sliptest.Function{
		Source: `(fceiling 3.5d+0)`,
		Expect: "4d+00, -5d-01",
	}).Test(t)
	(&sliptest.Function{
		Source: `(fceiling 5.5d+0 2)`,
		Expect: "3d+00, -5d-01",
	}).Test(t)
	(&sliptest.Function{
		Source: `(fceiling 1.5d+0 2)`,
		Expect: "1d+00, -5d-01",
	}).Test(t)
	(&sliptest.Function{
		Source: `(fceiling -5.5d+0 2)`,
		Expect: "-2d+00, -1.5d+00",
	}).Test(t)
}

func TestFceilingLongFloat(t *testing.T) {
	key := slip.Symbol("*print-readably*")
	orig, _ := slip.GetVar(key)
	slip.SetVar(key, slip.True)
	defer slip.SetVar(key, orig)

	(&sliptest.Function{
		Source: `(fceiling 3.5L+0)`,
		Expect: "4L+00, -5L-01",
	}).Test(t)
	(&sliptest.Function{
		Source: `(fceiling 5.5L+0 2)`,
		Expect: "3L+00, -5L-01",
	}).Test(t)
	(&sliptest.Function{
		Source: `(fceiling 1.5L+0 2)`,
		Expect: "1L+00, -5L-01",
	}).Test(t)
	(&sliptest.Function{
		Source: `(fceiling -5.5L+0 2)`,
		Expect: "-2L+00, -1.5L+00",
	}).Test(t)
	(&sliptest.Function{
		Source: `(fceiling 6.0L+0 2)`,
		Expect: "3L+00, 0L+00",
	}).Test(t)
}

func TestFceilingBignum(t *testing.T) {
	key := slip.Symbol("*print-readably*")
	orig, _ := slip.GetVar(key)
	slip.SetVar(key, slip.True)
	defer slip.SetVar(key, orig)

	(&sliptest.Function{
		Source: `(fceiling 50000000000000000000 20000000000000000000)`,
		Expect: "3d+00, -10000000000000000000",
	}).Test(t)
	(&sliptest.Function{
		Source: `(fceiling -50000000000000000000 20000000000000000000)`,
		Expect: "-2d+00, -10000000000000000000",
	}).Test(t)
	(&sliptest.Function{
		Source: `(fceiling 60000000000000000000 20000000000000000000)`,
		Expect: "3d+00, 0",
	}).Test(t)
	(&sliptest.Function{
		Source: `(fceiling 50000000000000000000 2)`,
		Expect: "2.5L+19, 0",
	}).Test(t)
}

func TestFceilingRatio(t *testing.T) {
	key := slip.Symbol("*print-readably*")
	orig, _ := slip.GetVar(key)
	slip.SetVar(key, slip.True)
	defer slip.SetVar(key, orig)

	(&sliptest.Function{
		Source: `(fceiling 3/4)`,
		Expect: "1d+00, -1/4",
	}).Test(t)
	(&sliptest.Function{
		Source: `(fceiling 12/2 2)`,
		Expect: "3d+00, 0",
	}).Test(t)
	(&sliptest.Function{
		Source: `(fceiling -9/4)`,
		Expect: "-2d+00, -1/4",
	}).Test(t)
	(&sliptest.Function{
		Source: `(fceiling 3/4 1/2)`,
		Expect: "2d+00, -1/4",
	}).Test(t)
}

func TestFceilingComplex(t *testing.T) {
	(&sliptest.Function{
		Source: `(fceiling #C(1 2) #C(1 2))`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(fceiling #C(1 0) 1)`,
		Panics: true,
	}).Test(t)
}

func TestFceilingNotNumber(t *testing.T) {
	(&sliptest.Function{
		Source: `(fceiling t)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(fceiling t 1)`,
		Panics: true,
	}).Test(t)
}

func TestFceilingNoArgs(t *testing.T) {
	(&sliptest.Function{
		Source: `(fceiling)`,
		Panics: true,
	}).Test(t)
}
