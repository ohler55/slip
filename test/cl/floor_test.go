// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestFloorFixnum(t *testing.T) {
	(&sliptest.Function{
		Source: `(floor 3)`,
		Expect: "3, 0",
	}).Test(t)
	(&sliptest.Function{
		Source: `(floor 1 2)`,
		Expect: "0, 1",
	}).Test(t)
	(&sliptest.Function{
		Source: `(floor 5 2)`,
		Expect: "2, 1",
	}).Test(t)
	(&sliptest.Function{
		Source: `(floor -5 2)`,
		Expect: "-3, 1",
	}).Test(t)
	(&sliptest.Function{
		Source: `(floor -5 -2)`,
		Expect: "3, 1",
	}).Test(t)
	(&sliptest.Function{
		Source: `(floor 5 -2)`,
		Expect: "-2, 1",
	}).Test(t)
}

func TestFloorSingleFloat(t *testing.T) {
	key := slip.Symbol("*print-readably*")
	orig, _ := slip.GetVar(key)
	slip.SetVar(key, slip.True)
	defer slip.SetVar(key, orig)

	(&sliptest.Function{
		Source: `(floor 3.5s+0)`,
		Expect: "3, 5s-01",
	}).Test(t)
	(&sliptest.Function{
		Source: `(floor 5.5s+0 2)`,
		Expect: "2, 1.5s+00",
	}).Test(t)
	(&sliptest.Function{
		Source: `(floor 1.5s+0 2)`,
		Expect: "0, 1.5s+00",
	}).Test(t)
	(&sliptest.Function{
		Source: `(floor -5.5s+0 2)`,
		Expect: "-3, 5s-01",
	}).Test(t)
	(&sliptest.Function{
		Source: `(floor -5.5s+0 -2)`,
		Expect: "2, -1.5s+00",
	}).Test(t)
}

func TestFloorDoubleFloat(t *testing.T) {
	key := slip.Symbol("*print-readably*")
	orig, _ := slip.GetVar(key)
	slip.SetVar(key, slip.True)
	defer slip.SetVar(key, orig)

	(&sliptest.Function{
		Source: `(floor 3.5d+0)`,
		Expect: "3, 5d-01",
	}).Test(t)
	(&sliptest.Function{
		Source: `(floor 5.5d+0 2)`,
		Expect: "2, 1.5d+00",
	}).Test(t)
	(&sliptest.Function{
		Source: `(floor 1.5d+0 2)`,
		Expect: "0, 1.5d+00",
	}).Test(t)
	(&sliptest.Function{
		Source: `(floor -5.5d+0 2)`,
		Expect: "-3, 5d-01",
	}).Test(t)
	(&sliptest.Function{
		Source: `(floor 5.5d+0 -2)`,
		Expect: "-3, -5d-01",
	}).Test(t)
	(&sliptest.Function{
		Source: `(floor -5.5d+0 -2)`,
		Expect: "2, -1.5d+00",
	}).Test(t)
}

func TestFloorLongFloat(t *testing.T) {
	key := slip.Symbol("*print-readably*")
	orig, _ := slip.GetVar(key)
	slip.SetVar(key, slip.True)
	defer slip.SetVar(key, orig)

	(&sliptest.Function{
		Source: `(floor 3.5L+0)`,
		Expect: "3, 5L-01",
	}).Test(t)
	(&sliptest.Function{
		Source: `(floor 5.5L+0 2)`,
		Expect: "2, 1.5L+00",
	}).Test(t)
	(&sliptest.Function{
		Source: `(floor 4.0L+0 2)`,
		Expect: "2, 0L+00",
	}).Test(t)
	(&sliptest.Function{
		Source: `(floor -5.5L+0 -2)`,
		Expect: "2, -1.5L+00",
	}).Test(t)
	(&sliptest.Function{
		Source: `(floor 5.5L+0 -2)`,
		Expect: "-3, -5L-01",
	}).Test(t)
	(&sliptest.Function{
		Source: `(floor 1.5L+0 2)`,
		Expect: "0, 1.5L+00",
	}).Test(t)
	(&sliptest.Function{
		Source: `(floor -5.5L+0 2)`,
		Expect: "-3, 5L-01",
	}).Test(t)
}

func TestFloorBignum(t *testing.T) {
	(&sliptest.Function{
		Source: `(floor 50000000000000000000 20000000000000000000)`,
		Expect: "2, 10000000000000000000",
	}).Test(t)
	(&sliptest.Function{
		Source: `(floor -50000000000000000000 -20000000000000000000)`,
		Expect: "2, -10000000000000000000",
	}).Test(t)
	(&sliptest.Function{
		Source: `(floor 60000000000000000000 20000000000000000000)`,
		Expect: "3, 0",
	}).Test(t)
	(&sliptest.Function{
		Source: `(floor -50000000000000000000 20000000000000000000)`,
		Expect: "-3, 10000000000000000000",
	}).Test(t)
	(&sliptest.Function{
		Source: `(floor 50000000000000000000 -20000000000000000000)`,
		Expect: "-3, -10000000000000000000",
	}).Test(t)
}

func TestFloorRatio(t *testing.T) {
	(&sliptest.Function{
		Source: `(floor 3/4)`,
		Expect: "0, 3/4",
	}).Test(t)
	(&sliptest.Function{
		Source: `(floor 12/2 2)`,
		Expect: "3, 0",
	}).Test(t)
	(&sliptest.Function{
		Source: `(floor -9/4)`,
		Expect: "-3, 3/4",
	}).Test(t)
	(&sliptest.Function{
		Source: `(floor 3/4 1/2)`,
		Expect: "1, 1/4",
	}).Test(t)
	(&sliptest.Function{
		Source: `(floor 3/4 -1/2)`,
		Expect: "-2, -1/4",
	}).Test(t)
	(&sliptest.Function{
		Source: `(floor -3/4 1/2)`,
		Expect: "-2, 1/4",
	}).Test(t)
	(&sliptest.Function{
		Source: `(floor -3/4 -1/2)`,
		Expect: "1, -1/4",
	}).Test(t)
}

func TestFloorComplex(t *testing.T) {
	(&sliptest.Function{
		Source: `(floor #C(1 2) #C(1 2))`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(floor #C(1 0) 1)`,
		Panics: true,
	}).Test(t)
}

func TestFloorNotNumber(t *testing.T) {
	(&sliptest.Function{
		Source: `(floor t)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(floor t 1)`,
		Panics: true,
	}).Test(t)
}

func TestFloorNoArgs(t *testing.T) {
	(&sliptest.Function{
		Source: `(floor)`,
		Panics: true,
	}).Test(t)
}
