// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestRoundFixnum(t *testing.T) {
	(&sliptest.Function{
		Source: `(round 3)`,
		Expect: "3, 0",
	}).Test(t)
	(&sliptest.Function{
		Source: `(round 1 2)`,
		Expect: "0, 1",
	}).Test(t)
	(&sliptest.Function{
		Source: `(round 7 2)`,
		Expect: "4, -1",
	}).Test(t)
	(&sliptest.Function{
		Source: `(round -7 2)`,
		Expect: "-4, 1",
	}).Test(t)
	(&sliptest.Function{
		Source: `(round 5 2)`,
		Expect: "2, 1",
	}).Test(t)
	(&sliptest.Function{
		Source: `(round -5 2)`,
		Expect: "-2, -1",
	}).Test(t)
	(&sliptest.Function{
		Source: `(round 5 -2)`,
		Expect: "-2, 1",
	}).Test(t)
	(&sliptest.Function{
		Source: `(round -5 -2)`,
		Expect: "2, -1",
	}).Test(t)
}

func TestRoundSingleFloat(t *testing.T) {
	key := slip.Symbol("*print-readably*")
	orig, _ := slip.GetVar(key)
	slip.SetVar(key, slip.True)
	defer slip.SetVar(key, orig)

	(&sliptest.Function{
		Source: `(round 3.5s+0)`,
		Expect: "4, -5s-01",
	}).Test(t)
	(&sliptest.Function{
		Source: `(round 2.5s+0 2)`,
		Expect: "1, 5s-01",
	}).Test(t)
	(&sliptest.Function{
		Source: `(round 7.0s+0 2)`,
		Expect: "4, -1s+00",
	}).Test(t)
	(&sliptest.Function{
		Source: `(round -7.0s+0 2)`,
		Expect: "-4, 1s+00",
	}).Test(t)
	(&sliptest.Function{
		Source: `(round 5.0s+0 2)`,
		Expect: "2, 1s+00",
	}).Test(t)
	(&sliptest.Function{
		Source: `(round 5.5s+0 2)`,
		Expect: "3, -5s-01",
	}).Test(t)
	(&sliptest.Function{
		Source: `(round -5.5s+0 2)`,
		Expect: "-3, 5s-01",
	}).Test(t)
	(&sliptest.Function{
		Source: `(round 5.5s+0 -2)`,
		Expect: "-3, -5s-01",
	}).Test(t)
	(&sliptest.Function{
		Source: `(round -5.5s+0 -2)`,
		Expect: "3, 5s-01",
	}).Test(t)
}

func TestRoundDoubleFloat(t *testing.T) {
	key := slip.Symbol("*print-readably*")
	orig, _ := slip.GetVar(key)
	slip.SetVar(key, slip.True)
	defer slip.SetVar(key, orig)

	(&sliptest.Function{
		Source: `(round 3.5d+0)`,
		Expect: "4, -5d-01",
	}).Test(t)
	(&sliptest.Function{
		Source: `(round 2.5d+0 2)`,
		Expect: "1, 5d-01",
	}).Test(t)
	(&sliptest.Function{
		Source: `(round 7.0d+0 2)`,
		Expect: "4, -1d+00",
	}).Test(t)
	(&sliptest.Function{
		Source: `(round -7.0d+0 2)`,
		Expect: "-4, 1d+00",
	}).Test(t)
	(&sliptest.Function{
		Source: `(round 5.0d+0 2)`,
		Expect: "2, 1d+00",
	}).Test(t)
	(&sliptest.Function{
		Source: `(round 5.5d+0 2)`,
		Expect: "3, -5d-01",
	}).Test(t)
	(&sliptest.Function{
		Source: `(round -5.5d+0 2)`,
		Expect: "-3, 5d-01",
	}).Test(t)
	(&sliptest.Function{
		Source: `(round 5.5d+0 -2)`,
		Expect: "-3, -5d-01",
	}).Test(t)
	(&sliptest.Function{
		Source: `(round -5.5d+0 -2)`,
		Expect: "3, 5d-01",
	}).Test(t)
}

func TestRoundLongFloat(t *testing.T) {
	key := slip.Symbol("*print-readably*")
	orig, _ := slip.GetVar(key)
	slip.SetVar(key, slip.True)
	defer slip.SetVar(key, orig)

	(&sliptest.Function{
		Source: `(round 3.5L+0)`,
		Expect: "4, -5L-01",
	}).Test(t)
	(&sliptest.Function{
		Source: `(round 1.5L+0 2)`,
		Expect: "1, -5L-01",
	}).Test(t)
	(&sliptest.Function{
		Source: `(round 6.0L+0 2)`,
		Expect: "3, 0L+00",
	}).Test(t)
	(&sliptest.Function{
		Source: `(round 5.5L+0 2)`,
		Expect: "3, -5L-01",
	}).Test(t)
	(&sliptest.Function{
		Source: `(round -5.5L+0 2)`,
		Expect: "-3, 5L-01",
	}).Test(t)
	(&sliptest.Function{
		Source: `(round 5.5L+0 -2)`,
		Expect: "-3, -5L-01",
	}).Test(t)
	(&sliptest.Function{
		Source: `(round -5.5L+0 -2)`,
		Expect: "3, 5L-01",
	}).Test(t)
	(&sliptest.Function{
		Source: `(round 4.5L+0 2)`,
		Expect: "2, 5L-01",
	}).Test(t)
	(&sliptest.Function{
		Source: `(round 4.5L+0 -2)`,
		Expect: "-2, 5L-01",
	}).Test(t)
	(&sliptest.Function{
		Source: `(round -4.5L+0 2)`,
		Expect: "-2, -5L-01",
	}).Test(t)
	(&sliptest.Function{
		Source: `(round -4.5L+0 -2)`,
		Expect: "2, -5L-01",
	}).Test(t)
}

func TestRoundBignum(t *testing.T) {
	(&sliptest.Function{
		Source: `(round 60000000000000000000 20000000000000000000)`,
		Expect: "3, 0",
	}).Test(t)
	(&sliptest.Function{
		Source: `(round 50000000000000000000 20000000000000000000)`,
		Expect: "2, 10000000000000000000",
	}).Test(t)
	(&sliptest.Function{
		Source: `(round 70000000000000000000 20000000000000000000)`,
		Expect: "4, -10000000000000000000",
	}).Test(t)
	(&sliptest.Function{
		Source: `(round 55000000000000000000 20000000000000000000)`,
		Expect: "3, -5000000000000000000",
	}).Test(t)
	(&sliptest.Function{
		Source: `(round -50000000000000000000 20000000000000000000)`,
		Expect: "-2, -10000000000000000000",
	}).Test(t)
	(&sliptest.Function{
		Source: `(round 50000000000000000000 -20000000000000000000)`,
		Expect: "-2, 10000000000000000000",
	}).Test(t)
	(&sliptest.Function{
		Source: `(round -50000000000000000000 -20000000000000000000)`,
		Expect: "2, -10000000000000000000",
	}).Test(t)
}

func TestRoundRatio(t *testing.T) {
	(&sliptest.Function{
		Source: `(round 3/4)`,
		Expect: "1, -1/4",
	}).Test(t)
	(&sliptest.Function{
		Source: `(round 12/2 2)`,
		Expect: "3, 0",
	}).Test(t)
	(&sliptest.Function{
		Source: `(round -9/4)`,
		Expect: "-2, -1/4",
	}).Test(t)
	(&sliptest.Function{
		Source: `(round 3/4 1/2)`,
		Expect: "2, -1/4",
	}).Test(t)
	(&sliptest.Function{
		Source: `(round 3/4 -1/2)`,
		Expect: "-2, -1/4",
	}).Test(t)
	(&sliptest.Function{
		Source: `(round -3/4 1/2)`,
		Expect: "-2, 1/4",
	}).Test(t)
	(&sliptest.Function{
		Source: `(round -3/4 -1/2)`,
		Expect: "2, 1/4",
	}).Test(t)
}

func TestRoundComplex(t *testing.T) {
	(&sliptest.Function{
		Source: `(round #C(1 2) #C(1 2))`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(round #C(1 0) 1)`,
		Panics: true,
	}).Test(t)
}

func TestRoundNotNumber(t *testing.T) {
	(&sliptest.Function{
		Source: `(round t)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(round t 1)`,
		Panics: true,
	}).Test(t)
}

func TestRoundNoArgs(t *testing.T) {
	(&sliptest.Function{
		Source: `(round)`,
		Panics: true,
	}).Test(t)
}
