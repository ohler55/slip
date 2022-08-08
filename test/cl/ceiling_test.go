// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestCeilingFixnum(t *testing.T) {
	(&sliptest.Function{
		Source: `(ceiling 5 2)`,
		Expect: "3, -1",
	}).Test(t)

	(&sliptest.Function{
		Source: `(ceiling 3)`,
		Expect: "3, 0",
	}).Test(t)
	(&sliptest.Function{
		Source: `(ceiling 5 2)`,
		Expect: "3, -1",
	}).Test(t)
	(&sliptest.Function{
		Source: `(ceiling -5 2)`,
		Expect: "-2, -1",
	}).Test(t)
}

func TestCeilingSingleFloat(t *testing.T) {
	key := slip.Symbol("*print-readably*")
	orig, _ := slip.GetVar(key)
	slip.SetVar(key, slip.True)
	defer slip.SetVar(key, orig)

	(&sliptest.Function{
		Source: `(ceiling 3.5s+0)`,
		Expect: "4s+00, -5s-01",
	}).Test(t)
	(&sliptest.Function{
		Source: `(ceiling 5.5s+0 2)`,
		Expect: "3s+00, -5s-01",
	}).Test(t)
	(&sliptest.Function{
		Source: `(ceiling -5.5s+0 2)`,
		Expect: "-2s+00, -1.5s+00",
	}).Test(t)
}

func TestCeilingDoubleFloat(t *testing.T) {
	key := slip.Symbol("*print-readably*")
	orig, _ := slip.GetVar(key)
	slip.SetVar(key, slip.True)
	defer slip.SetVar(key, orig)

	(&sliptest.Function{
		Source: `(ceiling 3.5d+0)`,
		Expect: "4d+00, -5d-01",
	}).Test(t)
	(&sliptest.Function{
		Source: `(ceiling 5.5d+0 2)`,
		Expect: "3d+00, -5d-01",
	}).Test(t)
	(&sliptest.Function{
		Source: `(ceiling -5.5d+0 2)`,
		Expect: "-2d+00, -1.5d+00",
	}).Test(t)
}

/*
func TestCeilingLongFloat(t *testing.T) {
	(&sliptest.Function{
		Source: `(ceiling 4 3.0L+0)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(ceiling 3.5s+0 3.5L+0)`,
		Expect: "nil",
	}).Test(t)
}

func TestCeilingBignum(t *testing.T) {
	(&sliptest.Function{
		Source: `(ceiling 30000000000000000000 20000000000000000000)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(ceiling (- 30000000000000000100 30000000000000000000) 100)`,
		Expect: "nil",
	}).Test(t)
}

func TestCeilingRatio(t *testing.T) {
	(&sliptest.Function{
		Source: `(ceiling 3/4 6/8)`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: `(ceiling 3/4 0.75)`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: `(ceiling 3/4 1/2)`,
		Expect: "t",
	}).Test(t)
}
*/
func TestCeilingComplex(t *testing.T) {
	(&sliptest.Function{
		Source: `(ceiling #C(1 2) #C(1 2))`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(ceiling #C(1 0) 1)`,
		Panics: true,
	}).Test(t)
}

func TestCeilingNotNumber(t *testing.T) {
	(&sliptest.Function{
		Source: `(ceiling t)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(ceiling t 1)`,
		Panics: true,
	}).Test(t)
}

func TestCeilingNoArgs(t *testing.T) {
	(&sliptest.Function{
		Source: `(ceiling)`,
		Panics: true,
	}).Test(t)
}
