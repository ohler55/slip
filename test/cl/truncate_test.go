// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestTruncateFixnum(t *testing.T) {
	(&sliptest.Function{
		Source: `(truncate 3)`,
		Expect: "3, 0",
	}).Test(t)
	(&sliptest.Function{
		Source: `(truncate 1 2)`,
		Expect: "0, 1",
	}).Test(t)
	(&sliptest.Function{
		Source: `(truncate 7 2)`,
		Expect: "3, 1",
	}).Test(t)
	(&sliptest.Function{
		Source: `(truncate 5 2)`,
		Expect: "2, 1",
	}).Test(t)
	(&sliptest.Function{
		Source: `(truncate -5 2)`,
		Expect: "-2, -1",
	}).Test(t)
	(&sliptest.Function{
		Source: `(truncate 5 -2)`,
		Expect: "-2, 1",
	}).Test(t)
	(&sliptest.Function{
		Source: `(truncate -5 -2)`,
		Expect: "2, -1",
	}).Test(t)
}

func TestTruncateSingleFloat(t *testing.T) {
	key := slip.Symbol("*print-readably*")
	orig, _ := slip.GetVar(key)
	slip.SetVar(key, slip.True)
	defer slip.SetVar(key, orig)

	(&sliptest.Function{
		Source: `(truncate 3.5s+0)`,
		Expect: "3, 5s-01",
	}).Test(t)
	(&sliptest.Function{
		Source: `(truncate 2.5s+0 2)`,
		Expect: "1, 5s-01",
	}).Test(t)
	(&sliptest.Function{
		Source: `(truncate 7.0s+0 2)`,
		Expect: "3, 1s+00",
	}).Test(t)
	(&sliptest.Function{
		Source: `(truncate 5.0s+0 2)`,
		Expect: "2, 1s+00",
	}).Test(t)
	(&sliptest.Function{
		Source: `(truncate 5.5s+0 2)`,
		Expect: "2, 1.5s+00",
	}).Test(t)
	(&sliptest.Function{
		Source: `(truncate -5.5s+0 2)`,
		Expect: "-2, -1.5s+00",
	}).Test(t)
	(&sliptest.Function{
		Source: `(truncate 5.5s+0 -2)`,
		Expect: "-2, 1.5s+00",
	}).Test(t)
	(&sliptest.Function{
		Source: `(truncate -5.5s+0 -2)`,
		Expect: "2, -1.5s+00",
	}).Test(t)
}

func TestTruncateDoubleFloat(t *testing.T) {
	key := slip.Symbol("*print-readably*")
	orig, _ := slip.GetVar(key)
	slip.SetVar(key, slip.True)
	defer slip.SetVar(key, orig)

	(&sliptest.Function{
		Source: `(truncate 3.5d+0)`,
		Expect: "3, 5d-01",
	}).Test(t)
	(&sliptest.Function{
		Source: `(truncate 1.5d+0 2)`,
		Expect: "0, 1.5d+00",
	}).Test(t)
	(&sliptest.Function{
		Source: `(truncate 5.5d+0 2)`,
		Expect: "2, 1.5d+00",
	}).Test(t)
	(&sliptest.Function{
		Source: `(truncate -5.5d+0 2)`,
		Expect: "-2, -1.5d+00",
	}).Test(t)
	(&sliptest.Function{
		Source: `(truncate 5.5d+0 -2)`,
		Expect: "-2, 1.5d+00",
	}).Test(t)
	(&sliptest.Function{
		Source: `(truncate -5.5d+0 -2)`,
		Expect: "2, -1.5d+00",
	}).Test(t)
}

func TestTruncateLongFloat(t *testing.T) {
	key := slip.Symbol("*print-readably*")
	orig, _ := slip.GetVar(key)
	slip.SetVar(key, slip.True)
	defer slip.SetVar(key, orig)

	(&sliptest.Function{
		Source: `(truncate 3.5L+0)`,
		Expect: "3, 5L-01",
	}).Test(t)
	(&sliptest.Function{
		Source: `(truncate 1.5L+0 2)`,
		Expect: "0, 1.5L+00",
	}).Test(t)
	(&sliptest.Function{
		Source: `(truncate 6.0L+0 2)`,
		Expect: "3, 0L+00",
	}).Test(t)
	(&sliptest.Function{
		Source: `(truncate 5.5L+0 2)`,
		Expect: "2, 1.5L+00",
	}).Test(t)
	(&sliptest.Function{
		Source: `(truncate -5.5L+0 2)`,
		Expect: "-2, -1.5L+00",
	}).Test(t)
	(&sliptest.Function{
		Source: `(truncate 5.5L+0 -2)`,
		Expect: "-2, 1.5L+00",
	}).Test(t)
	(&sliptest.Function{
		Source: `(truncate -5.5L+0 -2)`,
		Expect: "2, -1.5L+00",
	}).Test(t)
}

func TestTruncateBignum(t *testing.T) {
	(&sliptest.Function{
		Source: `(truncate 60000000000000000000 20000000000000000000)`,
		Expect: "3, 0",
	}).Test(t)
	(&sliptest.Function{
		Source: `(truncate 50000000000000000000 20000000000000000000)`,
		Expect: "2, 10000000000000000000",
	}).Test(t)
	(&sliptest.Function{
		Source: `(truncate -50000000000000000000 20000000000000000000)`,
		Expect: "-2, -10000000000000000000",
	}).Test(t)
	(&sliptest.Function{
		Source: `(truncate 50000000000000000000 -20000000000000000000)`,
		Expect: "-2, 10000000000000000000",
	}).Test(t)
	(&sliptest.Function{
		Source: `(truncate -50000000000000000000 -20000000000000000000)`,
		Expect: "2, -10000000000000000000",
	}).Test(t)
}

func TestTruncateRatio(t *testing.T) {
	(&sliptest.Function{
		Source: `(truncate 3/4)`,
		Expect: "0, 3/4",
	}).Test(t)
	(&sliptest.Function{
		Source: `(truncate 12/2 2)`,
		Expect: "3, 0",
	}).Test(t)
	(&sliptest.Function{
		Source: `(truncate -9/4)`,
		Expect: "-2, -1/4",
	}).Test(t)
	(&sliptest.Function{
		Source: `(truncate 3/4 1/2)`,
		Expect: "1, 1/4",
	}).Test(t)
	(&sliptest.Function{
		Source: `(truncate 3/4 -1/2)`,
		Expect: "-1, 1/4",
	}).Test(t)
	(&sliptest.Function{
		Source: `(truncate -3/4 1/2)`,
		Expect: "-1, -1/4",
	}).Test(t)
	(&sliptest.Function{
		Source: `(truncate -3/4 -1/2)`,
		Expect: "1, -1/4",
	}).Test(t)
}

func TestTruncateComplex(t *testing.T) {
	(&sliptest.Function{
		Source: `(truncate #C(1 2) #C(1 2))`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(truncate #C(1 0) 1)`,
		Panics: true,
	}).Test(t)
}

func TestTruncateNotNumber(t *testing.T) {
	(&sliptest.Function{
		Source: `(truncate t)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(truncate t 1)`,
		Panics: true,
	}).Test(t)
}

func TestTruncateNoArgs(t *testing.T) {
	(&sliptest.Function{
		Source: `(truncate)`,
		Panics: true,
	}).Test(t)
}
