// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestMinFixnum(t *testing.T) {
	(&sliptest.Function{
		Source: `(min 3)`,
		Expect: "3",
	}).Test(t)
	(&sliptest.Function{
		Source: `(min 3 4)`,
		Expect: "3",
	}).Test(t)
	(&sliptest.Function{
		Source: `(min 5 3 4)`,
		Expect: "3",
	}).Test(t)
}

func TestMinSingleFloat(t *testing.T) {
	key := slip.Symbol("*print-readably*")
	orig, _ := slip.GetVar(key)
	slip.SetVar(key, slip.True)
	defer slip.SetVar(key, orig)

	(&sliptest.Function{
		Source: `(min 3.0s+0 2)`,
		Expect: "2",
	}).Test(t)
	(&sliptest.Function{
		Source:   `(min 3.0s+0 4)`,
		Readably: true,
		Expect:   "3s+00",
	}).Test(t)
}

func TestMinDoubleFloat(t *testing.T) {
	key := slip.Symbol("*print-readably*")
	orig, _ := slip.GetVar(key)
	slip.SetVar(key, slip.True)
	defer slip.SetVar(key, orig)

	(&sliptest.Function{
		Source: `(min 3.0d+0 2)`,
		Expect: "2",
	}).Test(t)
	(&sliptest.Function{
		Source:   `(min 3.5s+0 2.5d+0)`,
		Readably: true,
		Expect:   "2.5d+00",
	}).Test(t)
}

func TestMinLongFloat(t *testing.T) {
	key := slip.Symbol("*print-readably*")
	orig, _ := slip.GetVar(key)
	slip.SetVar(key, slip.True)
	defer slip.SetVar(key, orig)

	(&sliptest.Function{
		Source:   `(min 2 1.0L+0)`,
		Readably: true,
		Expect:   "1L+00",
	}).Test(t)
	(&sliptest.Function{
		Source:   `(min 3.5s+0 3.5L+0)`,
		Readably: true,
		Expect:   "3.5s+00",
	}).Test(t)
}

func TestMinBignum(t *testing.T) {
	(&sliptest.Function{
		Source: `(min 30000000000000000000 40000000000000000000)`,
		Expect: "30000000000000000000",
	}).Test(t)
	(&sliptest.Function{
		Source: `(min (- 30000000000000000100 30000000000000000000) 100)`,
		Expect: "100",
	}).Test(t)
	(&sliptest.Function{
		Source: `(min (- 30000000000000000100 30000000000000000000) 99)`,
		Expect: "99",
	}).Test(t)
}

func TestMinRatio(t *testing.T) {
	(&sliptest.Function{
		Source: `(min 3/4 5/8)`,
		Expect: "5/8",
	}).Test(t)
	(&sliptest.Function{
		Source: `(min 3/4 0.74)`,
		Expect: "0.74",
	}).Test(t)
}

func TestMinComplex(t *testing.T) {
	(&sliptest.Function{
		Source: `(min 1 #C(1 2))`,
		Panics: true,
	}).Test(t)
}

func TestMinNotNumber(t *testing.T) {
	(&sliptest.Function{
		Source: `(min t)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(min t 1)`,
		Panics: true,
	}).Test(t)
}

func TestMinNoArgs(t *testing.T) {
	(&sliptest.Function{
		Source: `(min)`,
		Panics: true,
	}).Test(t)
}
