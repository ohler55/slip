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

func TestAddSingleFloat(t *testing.T) {
	key := slip.Symbol("*print-readably*")
	orig, _ := slip.GetVar(key)
	slip.SetVar(key, slip.True)
	defer slip.SetVar(key, orig)

	(&sliptest.Function{
		Source: `(+ 3.0s+0)`,
		Expect: "3s+00",
	}).Test(t)
	(&sliptest.Function{
		Source: `(+ 3.5s+0 4.5s+0 5.5s+0)`,
		Expect: "1.35s+01",
	}).Test(t)
}

/*
func TestAddDoubleFloat(t *testing.T) {
	(&sliptest.Function{
		Source: `(+ 3.1d0)`,
		Expect: "4.1",
	}).Test(t)
}

func TestAddLongFloat(t *testing.T) {
	(&sliptest.Function{
		Source: `(+ 3.5l0)`,
		Expect: "4.5",
	}).Test(t)
}

func TestAddRatio(t *testing.T) {
	(&sliptest.Function{
		Source: `(+ 3/4)`,
		Expect: "7/4",
	}).Test(t)
}

func TestAddBignum(t *testing.T) {
	(&sliptest.Function{
		Source: `(+ 100000000000000000000)`,
		Expect: "100000000000000000001",
	}).Test(t)
}

func TestAddComplex(t *testing.T) {
	(&sliptest.Function{
		Source: `(+ #C(1 2))`,
		Expect: "#C(2 2)",
	}).Test(t)
}
*/

func TestAddNotNumber(t *testing.T) {
	(&sliptest.Function{
		Source: `(+ t)`,
		Panics: true,
	}).Test(t)
}
