// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestSubtractFixnum(t *testing.T) {
	(&sliptest.Function{
		Source: `(- 3)`,
		Expect: "-3",
	}).Test(t)
	(&sliptest.Function{
		Source: `(- 5 4 3)`,
		Expect: "-2",
	}).Test(t)
}

func TestSubtractSingleFloat(t *testing.T) {
	key := slip.Symbol("*print-readably*")
	orig, _ := slip.GetVar(key)
	slip.SetVar(key, slip.True)
	defer slip.SetVar(key, orig)

	(&sliptest.Function{
		Source: `(- 3.0s+0)`,
		Expect: "-3s+00",
	}).Test(t)
	(&sliptest.Function{
		Source: `(- 9 3.0s+0 1.5s+0 1)`,
		Expect: "3.5s+00",
	}).Test(t)
}

func TestSubtractDoubleFloat(t *testing.T) {
	key := slip.Symbol("*print-readably*")
	orig, _ := slip.GetVar(key)
	slip.SetVar(key, slip.True)
	defer slip.SetVar(key, orig)

	(&sliptest.Function{
		Source: `(- 3.0d+0)`,
		Expect: "-3d+00",
	}).Test(t)
	(&sliptest.Function{
		Source: `(- 9 3.5d+0 1.5s+0 1)`,
		Expect: "3d+00",
	}).Test(t)
	(&sliptest.Function{
		Source: `(- 3.5s+0 4.5d+0 1.0d+0)`,
		Expect: "-2d+00",
	}).Test(t)
}

func TestSubtractLongFloat(t *testing.T) {
	key := slip.Symbol("*print-readably*")
	orig, _ := slip.GetVar(key)
	slip.SetVar(key, slip.True)
	defer slip.SetVar(key, orig)

	(&sliptest.Function{
		Source: `(- 3.0L+0)`,
		Expect: "-3L+00",
	}).Test(t)
	(&sliptest.Function{
		Source: `(- 9 3.5L+0 1.5s+0 1)`,
		Expect: "3L+00",
	}).Test(t)
	(&sliptest.Function{
		Source: `(- 3.5s+0 1.5L+0 1.0L+0)`,
		Expect: "1L+00",
	}).Test(t)
	(&sliptest.Function{
		Source: `(- 3.5d+0 4.5L+0 1.0d+0)`,
		Expect: "-2L+00",
	}).Test(t)
}

func TestSubtractBignum(t *testing.T) {
	key := slip.Symbol("*print-readably*")
	orig, _ := slip.GetVar(key)
	slip.SetVar(key, slip.True)
	defer slip.SetVar(key, orig)

	(&sliptest.Function{
		Source: `(- 30000000000000000000)`,
		Expect: "-30000000000000000000",
	}).Test(t)
	(&sliptest.Function{
		Source: `(- 1 30000000000000000000 2)`,
		Expect: "-30000000000000000001",
	}).Test(t)
	(&sliptest.Function{
		Source: `(- 30000000000000000000 20000000000000000000)`,
		Expect: "10000000000000000000",
	}).Test(t)
	(&sliptest.Function{
		Source: `(- (- 30000000000000001000 30000000000000000000)
                    2.0s+02
                    (- 10000000000000000100 10000000000000000000))`,
		Expect: "7s+02",
	}).Test(t)
	(&sliptest.Function{
		Source: `(- 30000000000000000000 2.0d+19 10000000000000000000)`,
		Expect: "0d+00",
	}).Test(t)
	(&sliptest.Function{
		Source: `(- 30000000000000000000 2.0L+19 10000000000000000000)`,
		Expect: "0L+00",
	}).Test(t)
	(&sliptest.Function{
		Source: `(- 2.0L+0 0.9999999999999999999L+0)`,
		Expect: "1.0000000000000000001L+00",
	}).Test(t)
}

func TestSubtractRatio(t *testing.T) {
	key := slip.Symbol("*print-readably*")
	orig, _ := slip.GetVar(key)
	slip.SetVar(key, slip.True)
	defer slip.SetVar(key, orig)

	(&sliptest.Function{
		Source: `(- 3/4)`,
		Expect: "-3/4",
	}).Test(t)
	(&sliptest.Function{
		Source: `(- 3/4 1/2)`,
		Expect: "1/4",
	}).Test(t)
	(&sliptest.Function{
		Source: `(- 1 3/4 1)`,
		Expect: "-3/4",
	}).Test(t)
	(&sliptest.Function{
		Source: `(- 1.5s+0 3/4)`,
		Expect: "7.5s-01",
	}).Test(t)
	(&sliptest.Function{
		Source: `(- 3/4 1.5s+0)`,
		Expect: "-7.5s-01",
	}).Test(t)
	(&sliptest.Function{
		Source: `(- 1.5d+0 3/4)`,
		Expect: "7.5d-01",
	}).Test(t)
	(&sliptest.Function{
		Source: `(- 3/4 1.5d+0)`,
		Expect: "-7.5d-01",
	}).Test(t)
	(&sliptest.Function{
		Source: `(- 1.5L+0 3/4)`,
		Expect: "7.5L-01",
	}).Test(t)
	(&sliptest.Function{
		Source: `(- 3/4 1.5L+0)`,
		Expect: "-7.5L-01",
	}).Test(t)
	(&sliptest.Function{
		Source: `(- 20000000000000000000 1000/3)`,
		Expect: "1.9999999999999999667L+19",
	}).Test(t)
	(&sliptest.Function{
		Source: `(-  1000/3 20000000000000000000)`,
		Expect: "-1.9999999999999999667L+19",
	}).Test(t)
	(&sliptest.Function{
		Source: `(-  (- 20000000000000001000 20000000000000000000)
                     1/3
                     (- 20000000000000001000 20000000000000000000))`,
		Expect: "-1/3",
	}).Test(t)
}

func TestSubtractComplex(t *testing.T) {
	key := slip.Symbol("*print-readably*")
	orig, _ := slip.GetVar(key)
	slip.SetVar(key, slip.True)
	defer slip.SetVar(key, orig)

	(&sliptest.Function{
		Source: `(- #C(1 2))`,
		Expect: "#C(-1d+00 -2d+00)",
	}).Test(t)
	(&sliptest.Function{
		Source: `(- #C(1 2) #C(1 1))`,
		Expect: "#C(0d+00 1d+00)",
	}).Test(t)
	(&sliptest.Function{
		Source: `(- #C(1 2) 1)`,
		Expect: "#C(0d+00 2d+00)",
	}).Test(t)
	(&sliptest.Function{
		Source: `(- 1.0s+0 #C(1 2) 1.5s+0)`,
		Expect: "#C(-1.5d+00 -2d+00)",
	}).Test(t)
	(&sliptest.Function{
		Source: `(- 1.0d+0 #C(1 2) 1.5d+0)`,
		Expect: "#C(-1.5d+00 -2d+00)",
	}).Test(t)
	(&sliptest.Function{
		Source: `(- 1.0L+0 #C(1 2) 1.5L+0)`,
		Expect: "#C(-1.5d+00 -2d+00)",
	}).Test(t)
	(&sliptest.Function{
		Source: `(- 10000000000000000000 #C(0 2) 10000000000000000000)`,
		Expect: "#C(0d+00 -2d+00)",
	}).Test(t)
	(&sliptest.Function{
		Source: `(- 1/2 #C(0 2) 1/4)`,
		Expect: "#C(2.5d-01 -2d+00)",
	}).Test(t)
}

func TestSubtractNotNumber(t *testing.T) {
	(&sliptest.Function{
		Source: `(- t)`,
		Panics: true,
	}).Test(t)
}

func TestSubtractNoArgs(t *testing.T) {
	(&sliptest.Function{
		Source: `(-)`,
		Panics: true,
	}).Test(t)
}
