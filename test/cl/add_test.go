// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestAddNoArgs(t *testing.T) {
	(&sliptest.Function{
		Source: `(+)`,
		Expect: "0",
	}).Test(t)
}

func TestAddFixnum(t *testing.T) {
	(&sliptest.Function{
		Source: `(+ 3)`,
		Expect: "3",
	}).Test(t)
	(&sliptest.Function{
		Source: `(+ 3 4 5)`,
		Expect: "12",
	}).Test(t)
}

func TestAddOctet(t *testing.T) {
	(&sliptest.Function{
		Source: `(+ (coerce 3 'octet))`,
		Expect: "3",
	}).Test(t)
	(&sliptest.Function{
		Source: `(+ (coerce 3 'octet) (coerce 4 'octet) (coerce 5 'octet))`,
		Expect: "12",
	}).Test(t)
}

func TestAddSingleFloat(t *testing.T) {
	key := slip.Symbol("*print-readably*")
	orig, _ := slip.GetVar(key)
	slip.SetVar(key, slip.True)
	defer slip.SetVar(key, orig)

	(&sliptest.Function{
		Source:   `(+ 3.0s+0)`,
		Readably: true,
		Expect:   "3s+00",
	}).Test(t)
	(&sliptest.Function{
		Source:   `(+ 2 3.0s+0 1.5s+0 1)`,
		Readably: true,
		Expect:   "7.5s+00",
	}).Test(t)
}

func TestAddDoubleFloat(t *testing.T) {
	key := slip.Symbol("*print-readably*")
	orig, _ := slip.GetVar(key)
	slip.SetVar(key, slip.True)
	defer slip.SetVar(key, orig)

	(&sliptest.Function{
		Source:   `(+ 3.0d+0)`,
		Readably: true,
		Expect:   "3d+00",
	}).Test(t)
	(&sliptest.Function{
		Source:   `(+ 2 3.5d+0 4.5s+0 1)`,
		Readably: true,
		Expect:   "1.1d+01",
	}).Test(t)
	(&sliptest.Function{
		Source:   `(+ 3.5s+0 4.5d+0 1.0d+0)`,
		Readably: true,
		Expect:   "9d+00",
	}).Test(t)
}

func TestAddLongFloat(t *testing.T) {
	key := slip.Symbol("*print-readably*")
	orig, _ := slip.GetVar(key)
	slip.SetVar(key, slip.True)
	defer slip.SetVar(key, orig)

	(&sliptest.Function{
		Source:   `(+ 3.0L+0)`,
		Readably: true,
		Expect:   "3L+00",
	}).Test(t)
	(&sliptest.Function{
		Source:   `(+ 2 3.5L+0 4.5s+0 1)`,
		Readably: true,
		Expect:   "1.1L+01",
	}).Test(t)
	(&sliptest.Function{
		Source:   `(+ 3.5s+0 4.5L+0 1.0L+0)`,
		Readably: true,
		Expect:   "9L+00",
	}).Test(t)
	(&sliptest.Function{
		Source:   `(+ 3.5d+0 4.5L+0 1.0d+0)`,
		Readably: true,
		Expect:   "9L+00",
	}).Test(t)
}

func TestAddBignum(t *testing.T) {
	key := slip.Symbol("*print-readably*")
	orig, _ := slip.GetVar(key)
	slip.SetVar(key, slip.True)
	defer slip.SetVar(key, orig)

	(&sliptest.Function{
		Source: `(+ 30000000000000000000)`,
		Expect: "30000000000000000000",
	}).Test(t)
	(&sliptest.Function{
		Source: `(+ 1 30000000000000000000 2)`,
		Expect: "30000000000000000003",
	}).Test(t)
	(&sliptest.Function{
		Source: `(+ 30000000000000000000 20000000000000000000)`,
		Expect: "50000000000000000000",
	}).Test(t)
	(&sliptest.Function{
		Source:   `(+ 30000000000000000000 2.0s+19 10000000000000000000)`,
		Readably: true,
		Expect:   "6s+19",
	}).Test(t)
	(&sliptest.Function{
		Source:   `(+ 30000000000000000000 2.0d+19 10000000000000000000)`,
		Readably: true,
		Expect:   "6d+19",
	}).Test(t)
	(&sliptest.Function{
		Source:   `(+ 30000000000000000000 2.0L+19 10000000000000000000)`,
		Readably: true,
		Expect:   "6L+19",
	}).Test(t)
	(&sliptest.Function{
		Source:   `(+ 2.0L+0 1.0000000000000000001L+0)`,
		Readably: true,
		Expect:   "3.0000000000000000001L+00",
	}).Test(t)
}

func TestAddRatio(t *testing.T) {
	key := slip.Symbol("*print-readably*")
	orig, _ := slip.GetVar(key)
	slip.SetVar(key, slip.True)
	defer slip.SetVar(key, orig)

	(&sliptest.Function{
		Source: `(+ 3/4)`,
		Expect: "3/4",
	}).Test(t)
	(&sliptest.Function{
		Source: `(+ 3/4 1/2)`,
		Expect: "5/4",
	}).Test(t)
	(&sliptest.Function{
		Source: `(+ 1 3/4 1)`,
		Expect: "11/4",
	}).Test(t)
	(&sliptest.Function{
		Source:   `(+ 1.5s+0 3/4)`,
		Readably: true,
		Expect:   "2.25s+00",
	}).Test(t)
	(&sliptest.Function{
		Source:   `(+ 3/4 1.5s+0)`,
		Readably: true,
		Expect:   "2.25s+00",
	}).Test(t)
	(&sliptest.Function{
		Source:   `(+ 1.5d+0 3/4)`,
		Readably: true,
		Expect:   "2.25d+00",
	}).Test(t)
	(&sliptest.Function{
		Source:   `(+ 3/4 1.5d+0)`,
		Readably: true,
		Expect:   "2.25d+00",
	}).Test(t)
	(&sliptest.Function{
		Source:   `(+ 1.5L+0 3/4)`,
		Readably: true,
		Expect:   "2.25L+00",
	}).Test(t)
	(&sliptest.Function{
		Source:   `(+ 3/4 1.5L+0)`,
		Readably: true,
		Expect:   "2.25L+00",
	}).Test(t)
	(&sliptest.Function{
		Source:   `(+ 20000000000000000000 1000/3)`,
		Readably: true,
		Expect:   "2.0000000000000000333L+19",
	}).Test(t)
	(&sliptest.Function{
		Source:   `(+  1000/3 20000000000000000000)`,
		Readably: true,
		Expect:   "2.0000000000000000333L+19",
	}).Test(t)
	(&sliptest.Function{
		Source: `(+  (+ 20000000000000001000 -20000000000000000000)
                     1/3
                     (+ 20000000000000001000 -20000000000000000000))`,
		Expect: "6001/3",
	}).Test(t)
}

func TestAddComplex(t *testing.T) {
	key := slip.Symbol("*print-readably*")
	orig, _ := slip.GetVar(key)
	slip.SetVar(key, slip.True)
	defer slip.SetVar(key, orig)

	(&sliptest.Function{
		Source: `(+ #C(1 2))`,
		Expect: "#C(1d+00 2d+00)",
	}).Test(t)
	(&sliptest.Function{
		Source: `(+ #C(1 2) #C(1 1))`,
		Expect: "#C(2d+00 3d+00)",
	}).Test(t)
	(&sliptest.Function{
		Source: `(+ #C(1 2) 1)`,
		Expect: "#C(2d+00 2d+00)",
	}).Test(t)
	(&sliptest.Function{
		Source: `(+ 1.0s+0 #C(1 2) 1.5s+0)`,
		Expect: "#C(3.5d+00 2d+00)",
	}).Test(t)
	(&sliptest.Function{
		Source: `(+ 1.0d+0 #C(1 2) 1.5d+0)`,
		Expect: "#C(3.5d+00 2d+00)",
	}).Test(t)
	(&sliptest.Function{
		Source: `(+ 1.0L+0 #C(1 2) 1.5L+0)`,
		Expect: "#C(3.5d+00 2d+00)",
	}).Test(t)
	(&sliptest.Function{
		Source: `(+ 10000000000000000000 #C(0 2) 10000000000000000000)`,
		Expect: "#C(2d+19 2d+00)",
	}).Test(t)
	(&sliptest.Function{
		Source: `(+ 1/2 #C(0 2) 1/4)`,
		Expect: "#C(7.5d-01 2d+00)",
	}).Test(t)
}

func TestAddNotNumber(t *testing.T) {
	(&sliptest.Function{
		Source: `(+ t)`,
		Panics: true,
	}).Test(t)
}
