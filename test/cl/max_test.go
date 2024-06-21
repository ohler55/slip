// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestMaxFixnum(t *testing.T) {
	(&sliptest.Function{
		Source: `(max 3)`,
		Expect: "3",
	}).Test(t)
	(&sliptest.Function{
		Source: `(max 3 4)`,
		Expect: "4",
	}).Test(t)
	(&sliptest.Function{
		Source: `(max 5 3 4)`,
		Expect: "5",
	}).Test(t)
}

func TestMaxSingleFloat(t *testing.T) {
	key := slip.Symbol("*print-readably*")
	orig, _ := slip.GetVar(key)
	slip.SetVar(key, slip.True)
	defer slip.SetVar(key, orig)

	(&sliptest.Function{
		Source: `(max 3.0s+0 4)`,
		Expect: "4",
	}).Test(t)
	(&sliptest.Function{
		Source:   `(max 3.0s+0 2)`,
		Readably: true,
		Expect:   "3s+00",
	}).Test(t)
}

func TestMaxDoubleFloat(t *testing.T) {
	key := slip.Symbol("*print-readably*")
	orig, _ := slip.GetVar(key)
	slip.SetVar(key, slip.True)
	defer slip.SetVar(key, orig)

	(&sliptest.Function{
		Source: `(max 3.0d+0 4)`,
		Expect: "4",
	}).Test(t)
	(&sliptest.Function{
		Source:   `(max 3.5s+0 4.5d+0 4)`,
		Readably: true,
		Expect:   "4.5d+00",
	}).Test(t)
}

func TestMaxLongFloat(t *testing.T) {
	key := slip.Symbol("*print-readably*")
	orig, _ := slip.GetVar(key)
	slip.SetVar(key, slip.True)
	defer slip.SetVar(key, orig)

	(&sliptest.Function{
		Source:   `(max 2 3.0L+0)`,
		Readably: true,
		Expect:   "3L+00",
	}).Test(t)
	(&sliptest.Function{
		Source:   `(max 3.5s+0 3.5L+0)`,
		Readably: true,
		Expect:   "3.5s+00",
	}).Test(t)
}

func TestMaxBignum(t *testing.T) {
	(&sliptest.Function{
		Source: `(max 30000000000000000000 40000000000000000000)`,
		Expect: "40000000000000000000",
	}).Test(t)
	(&sliptest.Function{
		Source: `(max (- 30000000000000000100 30000000000000000000) 100)`,
		Expect: "100",
	}).Test(t)
	(&sliptest.Function{
		Source: `(max (- 30000000000000000100 30000000000000000000) 99)`,
		Expect: "100",
	}).Test(t)
}

func TestMaxRatio(t *testing.T) {
	(&sliptest.Function{
		Source: `(max 3/4 7/8)`,
		Expect: "7/8",
	}).Test(t)
	(&sliptest.Function{
		Source: `(max 3/4 0.76)`,
		Expect: "0.76",
	}).Test(t)
}

func TestMaxComplex(t *testing.T) {
	(&sliptest.Function{
		Source: `(max 1 #C(1 2))`,
		Panics: true,
	}).Test(t)
}

func TestMaxNotNumber(t *testing.T) {
	(&sliptest.Function{
		Source: `(max t)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(max t 1)`,
		Panics: true,
	}).Test(t)
}

func TestMaxNoArgs(t *testing.T) {
	(&sliptest.Function{
		Source: `(max)`,
		Panics: true,
	}).Test(t)
}
