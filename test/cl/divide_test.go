// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestDivideFixnum(t *testing.T) {
	(&sliptest.Function{
		Source: `(/ 1)`,
		Expect: "1",
	}).Test(t)
	(&sliptest.Function{
		Source: `(/ 3)`,
		Expect: "1/3",
	}).Test(t)
	(&sliptest.Function{
		Source: `(/ 10 2)`,
		Expect: "5",
	}).Test(t)
	(&sliptest.Function{
		Source: `(/ 5 4 3)`,
		Expect: "5/12",
	}).Test(t)
	(&sliptest.Function{
		Source:    `(/ 0)`,
		PanicType: slip.DivisionByZeroSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(/ 5 0)`,
		PanicType: slip.DivisionByZeroSymbol,
	}).Test(t)
}

func TestDivideSingleFloat(t *testing.T) {
	key := slip.Symbol("*print-readably*")
	orig, _ := slip.GetVar(key)
	slip.SetVar(key, slip.True)
	defer slip.SetVar(key, orig)

	(&sliptest.Function{
		Source:   `(/ 2.0s+0)`,
		Readably: true,
		Expect:   "5s-01",
	}).Test(t)
	(&sliptest.Function{
		Source:   `(/ 9 3.0s+0 1.5s+0 1)`,
		Readably: true,
		Expect:   "2s+00",
	}).Test(t)
	(&sliptest.Function{
		Source:    `(/ 0.0s+0)`,
		PanicType: slip.DivisionByZeroSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(/ 3.0s+0 0.0s+0)`,
		PanicType: slip.DivisionByZeroSymbol,
	}).Test(t)
}

func TestDivideDoubleFloat(t *testing.T) {
	key := slip.Symbol("*print-readably*")
	orig, _ := slip.GetVar(key)
	slip.SetVar(key, slip.True)
	defer slip.SetVar(key, orig)

	(&sliptest.Function{
		Source:   `(/ 2.0d+0)`,
		Readably: true,
		Expect:   "5d-01",
	}).Test(t)
	(&sliptest.Function{
		Source:   `(/ 9 3.0d+0 1.5s+0 1)`,
		Readably: true,
		Expect:   "2d+00",
	}).Test(t)
	(&sliptest.Function{
		Source:   `(/ 3.0s+0 1.5d+0 1.0d+0)`,
		Readably: true,
		Expect:   "2d+00",
	}).Test(t)
	(&sliptest.Function{
		Source:    `(/ 0.0d+0)`,
		PanicType: slip.DivisionByZeroSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(/ 3.0d+0 0.0d+0)`,
		PanicType: slip.DivisionByZeroSymbol,
	}).Test(t)
}

func TestDivideLongFloat(t *testing.T) {
	key := slip.Symbol("*print-readably*")
	orig, _ := slip.GetVar(key)
	slip.SetVar(key, slip.True)
	defer slip.SetVar(key, orig)

	(&sliptest.Function{
		Source:   `(/ 2.0L+0)`,
		Readably: true,
		Expect:   "5L-01",
	}).Test(t)
	(&sliptest.Function{
		Source:   `(/ 9 3.0L+0 1.5s+0 1)`,
		Readably: true,
		Expect:   "2L+00",
	}).Test(t)
	(&sliptest.Function{
		Source:   `(/ 3.0s+0 1.5L+0 1.0L+0)`,
		Readably: true,
		Expect:   "2L+00",
	}).Test(t)
	(&sliptest.Function{
		Source:   `(/ 3.0d+0 1.5L+0 1.0d+0)`,
		Readably: true,
		Expect:   "2L+00",
	}).Test(t)
	(&sliptest.Function{
		Source:    `(/ 0.0L+0)`,
		PanicType: slip.DivisionByZeroSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(/ 3.0L+0 0.0L+0)`,
		PanicType: slip.DivisionByZeroSymbol,
	}).Test(t)
}

func TestDivideBignum(t *testing.T) {
	key := slip.Symbol("*print-readably*")
	orig, _ := slip.GetVar(key)
	slip.SetVar(key, slip.True)
	defer slip.SetVar(key, orig)

	(&sliptest.Function{
		Source: `(/ 20000000000000000000)`,
		Expect: "1/20000000000000000000",
	}).Test(t)
	(&sliptest.Function{
		Source: `(/ 20000000000000000000 10000000000000000000)`,
		Expect: "2",
	}).Test(t)
	(&sliptest.Function{
		Source: `(/ (- 20000000000000000001 20000000000000000000))`,
		Expect: "1",
	}).Test(t)
	(&sliptest.Function{
		Source: `(/ 1 30000000000000000000 2)`,
		Expect: "1/60000000000000000000",
	}).Test(t)
	(&sliptest.Function{
		Source: `(/ 30000000000000000000 20000000000000000000)`,
		Expect: "3/2",
	}).Test(t)
	(&sliptest.Function{
		Source: `(/ (- 30000000000000001000 30000000000000000000)
                    2.0s+02
                    (- 10000000000000000100 10000000000000000000))`,
		Readably: true,
		Expect:   "5s-02",
	}).Test(t)
	(&sliptest.Function{
		Source:   `(/ 30000000000000000000 2.0d+00 10000000000000000000)`,
		Readably: true,
		Expect:   "1.5d+00",
	}).Test(t)
	(&sliptest.Function{
		Source:   `(/ 30000000000000000000 2.0L+00 10000000000000000000)`,
		Readably: true,
		Expect:   "1.5L+00",
	}).Test(t)
	(&sliptest.Function{
		Source:   `(/ 4.0L+0 2.0000000000000000L+0)`,
		Readably: true,
		Expect:   "2L+00",
	}).Test(t)
	(&sliptest.Function{
		Source:    `(/ (- 20000000000000000000 20000000000000000000))`,
		PanicType: slip.DivisionByZeroSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(/ 10000000000000000000 (- 20000000000000000000 20000000000000000000))`,
		PanicType: slip.DivisionByZeroSymbol,
	}).Test(t)
}

func TestDivideRatio(t *testing.T) {
	key := slip.Symbol("*print-readably*")
	orig, _ := slip.GetVar(key)
	slip.SetVar(key, slip.True)
	defer slip.SetVar(key, orig)

	(&sliptest.Function{
		Source: `(/ 3/4)`,
		Expect: "4/3",
	}).Test(t)
	(&sliptest.Function{
		Source: `(/ 3/4 1/2)`,
		Expect: "3/2",
	}).Test(t)
	(&sliptest.Function{
		Source: `(/ 1 3/4 2)`,
		Expect: "2/3",
	}).Test(t)
	(&sliptest.Function{
		Source:   `(/ 1.5s+0 3/4)`,
		Readably: true,
		Expect:   "2s+00",
	}).Test(t)
	(&sliptest.Function{
		Source:   `(/ 3/4 1.5s+0)`,
		Readably: true,
		Expect:   "5s-01",
	}).Test(t)
	(&sliptest.Function{
		Source:   `(/ 1.5d+0 3/4)`,
		Readably: true,
		Expect:   "2d+00",
	}).Test(t)
	(&sliptest.Function{
		Source:   `(/ 3/4 1.5d+0)`,
		Readably: true,
		Expect:   "5d-01",
	}).Test(t)
	(&sliptest.Function{
		Source:   `(/ 1.5L+0 3/4)`,
		Readably: true,
		Expect:   "2L+00",
	}).Test(t)
	(&sliptest.Function{
		Source:   `(/ 3/4 1.5L+0)`,
		Readably: true,
		Expect:   "5L-01",
	}).Test(t)
	(&sliptest.Function{
		Source:   `(/ 20000000000000000000 1000/3)`,
		Readably: true,
		Expect:   "6L+16",
	}).Test(t)
	(&sliptest.Function{
		Source:   `(/  3/1000 20000000000000000000)`,
		Readably: true,
		Expect:   "1.5L-22",
	}).Test(t)
	(&sliptest.Function{
		Source: `(/  (/ 20000000000000001000 20000000000000000000)
                     1/3
                     (/ 20000000000000001000 20000000000000000000))`,
		Expect: "3",
	}).Test(t)
	(&sliptest.Function{
		Source:    `(/ 0/2)`,
		PanicType: slip.DivisionByZeroSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(/ 1/2 0/2)`,
		PanicType: slip.DivisionByZeroSymbol,
	}).Test(t)
}

func TestDivideComplex(t *testing.T) {
	key := slip.Symbol("*print-readably*")
	orig, _ := slip.GetVar(key)
	slip.SetVar(key, slip.True)
	defer slip.SetVar(key, orig)

	(&sliptest.Function{
		Source: `(/ #C(1 2))`,
		Expect: "#C(2d-01 -4d-01)",
	}).Test(t)
	(&sliptest.Function{
		Source: `(/ #C(1 2) #C(1 1))`,
		Expect: "#C(1.5d+00 5d-01)",
	}).Test(t)
	(&sliptest.Function{
		Source: `(/ #C(1 2) 1)`,
		Expect: "#C(1d+00 2d+00)",
	}).Test(t)
	(&sliptest.Function{
		Source: `(/ 1.0s+0 #C(1 2) 0.5s+0)`,
		Expect: "#C(4d-01 -8d-01)",
	}).Test(t)
	(&sliptest.Function{
		Source: `(/ 1.0d+0 #C(1 2) 0.5d+0)`,
		Expect: "#C(4d-01 -8d-01)",
	}).Test(t)
	(&sliptest.Function{
		Source: `(/ 1.0L+0 #C(1 2) 0.5L+0)`,
		Expect: "#C(4d-01 -8d-01)",
	}).Test(t)
	(&sliptest.Function{
		Source: `(/ 10000000000000000000 #C(0 2) 10000000000000000000)`,
		Expect: "#C(0d+00 -5d-01)",
	}).Test(t)
	(&sliptest.Function{
		Source: `(/ 1/2 #C(0 2) 1/4)`,
		Expect: "#C(0d+00 -1d+00)",
	}).Test(t)
}

func TestDivideNotNumber(t *testing.T) {
	(&sliptest.Function{
		Source: `(/ t)`,
		Panics: true,
	}).Test(t)
}

func TestDivideNoArgs(t *testing.T) {
	(&sliptest.Function{
		Source: `(/)`,
		Panics: true,
	}).Test(t)
}
