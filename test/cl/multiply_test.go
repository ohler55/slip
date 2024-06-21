// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestMultiplyNoArgs(t *testing.T) {
	(&sliptest.Function{
		Source: `(*)`,
		Expect: "1",
	}).Test(t)
}

func TestMultiplyFixnum(t *testing.T) {
	(&sliptest.Function{
		Source: `(* 3)`,
		Expect: "3",
	}).Test(t)
	(&sliptest.Function{
		Source: `(* 3 4 5)`,
		Expect: "60",
	}).Test(t)
}

func TestMultiplySingleFloat(t *testing.T) {
	key := slip.Symbol("*print-readably*")
	orig, _ := slip.GetVar(key)
	slip.SetVar(key, slip.True)
	defer slip.SetVar(key, orig)

	(&sliptest.Function{
		Source:   `(* 3.0s+0)`,
		Readably: true,
		Expect:   "3s+00",
	}).Test(t)
	(&sliptest.Function{
		Source:   `(* 2 3.0s+0 1.5s+0 1)`,
		Readably: true,
		Expect:   "9s+00",
	}).Test(t)
}

func TestMultiplyDoubleFloat(t *testing.T) {
	key := slip.Symbol("*print-readably*")
	orig, _ := slip.GetVar(key)
	slip.SetVar(key, slip.True)
	defer slip.SetVar(key, orig)

	(&sliptest.Function{
		Source:   `(* 3.0d+0)`,
		Readably: true,
		Expect:   "3d+00",
	}).Test(t)
	(&sliptest.Function{
		Source:   `(* 2 3.5d+0 1.5s+0 1)`,
		Readably: true,
		Expect:   "1.05d+01",
	}).Test(t)
	(&sliptest.Function{
		Source:   `(* 3.5s+0 1.5d+0 2.0d+0)`,
		Readably: true,
		Expect:   "1.05d+01",
	}).Test(t)
}

func TestMultiplyLongFloat(t *testing.T) {
	key := slip.Symbol("*print-readably*")
	orig, _ := slip.GetVar(key)
	slip.SetVar(key, slip.True)
	defer slip.SetVar(key, orig)

	(&sliptest.Function{
		Source:   `(* 3.0L+0)`,
		Readably: true,
		Expect:   "3L+00",
	}).Test(t)
	(&sliptest.Function{
		Source:   `(* 2 3.5L+0 1.5s+0 1)`,
		Readably: true,
		Expect:   "1.05L+01",
	}).Test(t)
	(&sliptest.Function{
		Source:   `(* 3.5s+0 1.5L+0 2.0L+0)`,
		Readably: true,
		Expect:   "1.05L+01",
	}).Test(t)
	(&sliptest.Function{
		Source:   `(* 3.5d+0 1.5L+0 2.0d+0)`,
		Readably: true,
		Expect:   "1.05L+01",
	}).Test(t)
}

func TestMultiplyBignum(t *testing.T) {
	key := slip.Symbol("*print-readably*")
	orig, _ := slip.GetVar(key)
	slip.SetVar(key, slip.True)
	defer slip.SetVar(key, orig)

	(&sliptest.Function{
		Source: `(* 30000000000000000000)`,
		Expect: "30000000000000000000",
	}).Test(t)
	(&sliptest.Function{
		Source: `(* 1 30000000000000000000 2)`,
		Expect: "60000000000000000000",
	}).Test(t)
	(&sliptest.Function{
		Source: `(* 30000000000000000000 20000000000000000000)`,
		Expect: "600000000000000000000000000000000000000",
	}).Test(t)
	(&sliptest.Function{
		Source: `(* (- 30000000000000003000 30000000000000000000)
                    2.0s+00
                    (- 10000000000000000100 10000000000000000000))`,
		Readably: true,
		Expect:   "6s+05",
	}).Test(t)
	(&sliptest.Function{
		Source: `(* (- 30000000000000003000 30000000000000000000)
                    2.0d+00
                    (- 10000000000000000100 10000000000000000000))`,
		Readably: true,
		Expect:   "6d+05",
	}).Test(t)
	(&sliptest.Function{
		Source:   `(* 30000000000000000000 2.0L+02 10000000000000000000)`,
		Readably: true,
		Expect:   "6L+40",
	}).Test(t)
	(&sliptest.Function{
		Source:   `(* 2.0L+0 1.0000000000000000001L+0)`,
		Readably: true,
		Expect:   "2.0000000000000000002L+00",
	}).Test(t)
}

func TestMultiplyRatio(t *testing.T) {
	key := slip.Symbol("*print-readably*")
	orig, _ := slip.GetVar(key)
	slip.SetVar(key, slip.True)
	defer slip.SetVar(key, orig)

	(&sliptest.Function{
		Source: `(* 3/4)`,
		Expect: "3/4",
	}).Test(t)
	(&sliptest.Function{
		Source: `(* 3/4 1/2)`,
		Expect: "3/8",
	}).Test(t)
	(&sliptest.Function{
		Source: `(* 2 3/4 1)`,
		Expect: "3/2",
	}).Test(t)
	(&sliptest.Function{
		Source:   `(* 1.5s+0 3/4)`,
		Readably: true,
		Expect:   "1.125s+00",
	}).Test(t)
	(&sliptest.Function{
		Source:   `(* 3/4 1.5s+0)`,
		Readably: true,
		Expect:   "1.125s+00",
	}).Test(t)
	(&sliptest.Function{
		Source:   `(* 1.5d+0 3/4)`,
		Readably: true,
		Expect:   "1.125d+00",
	}).Test(t)
	(&sliptest.Function{
		Source:   `(* 3/4 1.5d+0)`,
		Readably: true,
		Expect:   "1.125d+00",
	}).Test(t)
	(&sliptest.Function{
		Source:   `(* 1.5L+0 3/4)`,
		Readably: true,
		Expect:   "1.125L+00",
	}).Test(t)
	(&sliptest.Function{
		Source:   `(* 3/4 1.5L+0)`,
		Readably: true,
		Expect:   "1.125L+00",
	}).Test(t)
	(&sliptest.Function{
		Source:   `(* 20000000000000000000 3/2)`,
		Readably: true,
		Expect:   "3L+19",
	}).Test(t)
	(&sliptest.Function{
		Source:   `(*  3/2 20000000000000000000)`,
		Readably: true,
		Expect:   "3L+19",
	}).Test(t)
	(&sliptest.Function{
		Source: `(* (- 20000000000000001000 20000000000000000000)
                    1/3
                    (- 20000000000000001000 20000000000000000000))`,
		Expect: "1000000/3",
	}).Test(t)
}

func TestMultiplyComplex(t *testing.T) {
	key := slip.Symbol("*print-readably*")
	orig, _ := slip.GetVar(key)
	slip.SetVar(key, slip.True)
	defer slip.SetVar(key, orig)

	(&sliptest.Function{
		Source: `(* #C(1 2))`,
		Expect: "#C(1d+00 2d+00)",
	}).Test(t)
	(&sliptest.Function{
		Source: `(* #C(1 2) #C(1 1))`,
		Expect: "#C(-1d+00 3d+00)",
	}).Test(t)
	(&sliptest.Function{
		Source: `(* #C(1 2) 2)`,
		Expect: "#C(2d+00 4d+00)",
	}).Test(t)
	(&sliptest.Function{
		Source: `(* 1.0s+0 #C(1 2) 1.5s+0)`,
		Expect: "#C(1.5d+00 3d+00)",
	}).Test(t)
	(&sliptest.Function{
		Source: `(* 1.0d+0 #C(1 2) 1.5d+0)`,
		Expect: "#C(1.5d+00 3d+00)",
	}).Test(t)
	(&sliptest.Function{
		Source: `(* 1.0L+0 #C(1 2) 1.5L+0)`,
		Expect: "#C(1.5d+00 3d+00)",
	}).Test(t)
	(&sliptest.Function{
		Source: `(* 10000000000000000000 #C(0 2) 10000000000000000000)`,
		Expect: "#C(0d+00 2d+38)",
	}).Test(t)
	(&sliptest.Function{
		Source: `(* 1/2 #C(0 2) 1/4)`,
		Expect: "#C(0d+00 2.5d-01)",
	}).Test(t)
}

func TestMultiplyNotNumber(t *testing.T) {
	(&sliptest.Function{
		Source: `(* t)`,
		Panics: true,
	}).Test(t)
}
