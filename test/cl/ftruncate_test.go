// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestFtruncateFixnum(t *testing.T) {
	key := slip.Symbol("*print-readably*")
	orig, _ := slip.GetVar(key)
	slip.SetVar(key, slip.True)
	defer slip.SetVar(key, orig)

	(&sliptest.Function{
		Source: `(ftruncate 3)`,
		Expect: "3d+00, 0",
	}).Test(t)
	(&sliptest.Function{
		Source: `(ftruncate 1 2)`,
		Expect: "0d+00, 1",
	}).Test(t)
	(&sliptest.Function{
		Source: `(ftruncate 7 2)`,
		Expect: "3d+00, 1",
	}).Test(t)
	(&sliptest.Function{
		Source: `(ftruncate 5 2)`,
		Expect: "2d+00, 1",
	}).Test(t)
	(&sliptest.Function{
		Source: `(ftruncate -5 2)`,
		Expect: "-2d+00, -1",
	}).Test(t)
	(&sliptest.Function{
		Source: `(ftruncate 5 -2)`,
		Expect: "-2d+00, 1",
	}).Test(t)
	(&sliptest.Function{
		Source: `(ftruncate -5 -2)`,
		Expect: "2d+00, -1",
	}).Test(t)
}

func TestFtruncateSingleFloat(t *testing.T) {
	key := slip.Symbol("*print-readably*")
	orig, _ := slip.GetVar(key)
	slip.SetVar(key, slip.True)
	defer slip.SetVar(key, orig)

	(&sliptest.Function{
		Source: `(ftruncate 3.5s+0)`,
		Expect: "3s+00, 5s-01",
	}).Test(t)
	(&sliptest.Function{
		Source: `(ftruncate 2.5s+0 2)`,
		Expect: "1s+00, 5s-01",
	}).Test(t)
	(&sliptest.Function{
		Source: `(ftruncate 7.0s+0 2)`,
		Expect: "3s+00, 1s+00",
	}).Test(t)
	(&sliptest.Function{
		Source: `(ftruncate 5.0s+0 2)`,
		Expect: "2s+00, 1s+00",
	}).Test(t)
	(&sliptest.Function{
		Source: `(ftruncate 5.5s+0 2)`,
		Expect: "2s+00, 1.5s+00",
	}).Test(t)
	(&sliptest.Function{
		Source: `(ftruncate -5.5s+0 2)`,
		Expect: "-2s+00, -1.5s+00",
	}).Test(t)
	(&sliptest.Function{
		Source: `(ftruncate 5.5s+0 -2)`,
		Expect: "-2s+00, 1.5s+00",
	}).Test(t)
	(&sliptest.Function{
		Source: `(ftruncate -5.5s+0 -2)`,
		Expect: "2s+00, -1.5s+00",
	}).Test(t)
}

func TestFtruncateDoubleFloat(t *testing.T) {
	key := slip.Symbol("*print-readably*")
	orig, _ := slip.GetVar(key)
	slip.SetVar(key, slip.True)
	defer slip.SetVar(key, orig)

	(&sliptest.Function{
		Source: `(ftruncate 3.5d+0)`,
		Expect: "3d+00, 5d-01",
	}).Test(t)
	(&sliptest.Function{
		Source: `(ftruncate 1.5d+0 2)`,
		Expect: "0d+00, 1.5d+00",
	}).Test(t)
	(&sliptest.Function{
		Source: `(ftruncate 5.5d+0 2)`,
		Expect: "2d+00, 1.5d+00",
	}).Test(t)
	(&sliptest.Function{
		Source: `(ftruncate -5.5d+0 2)`,
		Expect: "-2d+00, -1.5d+00",
	}).Test(t)
	(&sliptest.Function{
		Source: `(ftruncate 5.5d+0 -2)`,
		Expect: "-2d+00, 1.5d+00",
	}).Test(t)
	(&sliptest.Function{
		Source: `(ftruncate -5.5d+0 -2)`,
		Expect: "2d+00, -1.5d+00",
	}).Test(t)
}

func TestFtruncateLongFloat(t *testing.T) {
	key := slip.Symbol("*print-readably*")
	orig, _ := slip.GetVar(key)
	slip.SetVar(key, slip.True)
	defer slip.SetVar(key, orig)

	(&sliptest.Function{
		Source: `(ftruncate 3.5L+0)`,
		Expect: "3L+00, 5L-01",
	}).Test(t)
	(&sliptest.Function{
		Source: `(ftruncate 1.5L+0 2)`,
		Expect: "0L+00, 1.5L+00",
	}).Test(t)
	(&sliptest.Function{
		Source: `(ftruncate 6.0L+0 2)`,
		Expect: "3L+00, 0L+00",
	}).Test(t)
	(&sliptest.Function{
		Source: `(ftruncate 5.5L+0 2)`,
		Expect: "2L+00, 1.5L+00",
	}).Test(t)
	(&sliptest.Function{
		Source: `(ftruncate -5.5L+0 2)`,
		Expect: "-2L+00, -1.5L+00",
	}).Test(t)
	(&sliptest.Function{
		Source: `(ftruncate 5.5L+0 -2)`,
		Expect: "-2L+00, 1.5L+00",
	}).Test(t)
	(&sliptest.Function{
		Source: `(ftruncate -5.5L+0 -2)`,
		Expect: "2L+00, -1.5L+00",
	}).Test(t)
}

func TestFtruncateBignum(t *testing.T) {
	key := slip.Symbol("*print-readably*")
	orig, _ := slip.GetVar(key)
	slip.SetVar(key, slip.True)
	defer slip.SetVar(key, orig)

	(&sliptest.Function{
		Source: `(ftruncate 60000000000000000000 20000000000000000000)`,
		Expect: "3d+00, 0",
	}).Test(t)
	(&sliptest.Function{
		Source: `(ftruncate 60000000000000000000 2)`,
		Expect: "3L+19, 0",
	}).Test(t)
	(&sliptest.Function{
		Source: `(ftruncate 50000000000000000000 20000000000000000000)`,
		Expect: "2d+00, 10000000000000000000",
	}).Test(t)
	(&sliptest.Function{
		Source: `(ftruncate -50000000000000000000 20000000000000000000)`,
		Expect: "-2d+00, -10000000000000000000",
	}).Test(t)
	(&sliptest.Function{
		Source: `(ftruncate 50000000000000000000 -20000000000000000000)`,
		Expect: "-2d+00, 10000000000000000000",
	}).Test(t)
	(&sliptest.Function{
		Source: `(ftruncate -50000000000000000000 -20000000000000000000)`,
		Expect: "2d+00, -10000000000000000000",
	}).Test(t)
}

func TestFtruncateRatio(t *testing.T) {
	key := slip.Symbol("*print-readably*")
	orig, _ := slip.GetVar(key)
	slip.SetVar(key, slip.True)
	defer slip.SetVar(key, orig)

	(&sliptest.Function{
		Source: `(ftruncate 3/4)`,
		Expect: "0d+00, 3/4",
	}).Test(t)
	(&sliptest.Function{
		Source: `(ftruncate 12/2 2)`,
		Expect: "3d+00, 0",
	}).Test(t)
	(&sliptest.Function{
		Source: `(ftruncate -9/4)`,
		Expect: "-2d+00, -1/4",
	}).Test(t)
	(&sliptest.Function{
		Source: `(ftruncate 3/4 1/2)`,
		Expect: "1d+00, 1/4",
	}).Test(t)
	(&sliptest.Function{
		Source: `(ftruncate 3/4 -1/2)`,
		Expect: "-1d+00, 1/4",
	}).Test(t)
	(&sliptest.Function{
		Source: `(ftruncate -3/4 1/2)`,
		Expect: "-1d+00, -1/4",
	}).Test(t)
	(&sliptest.Function{
		Source: `(ftruncate -3/4 -1/2)`,
		Expect: "1d+00, -1/4",
	}).Test(t)
}

func TestFtruncateComplex(t *testing.T) {
	(&sliptest.Function{
		Source: `(ftruncate #C(1 2) #C(1 2))`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(ftruncate #C(1 0) 1)`,
		Panics: true,
	}).Test(t)
}

func TestFtruncateNotNumber(t *testing.T) {
	(&sliptest.Function{
		Source: `(ftruncate t)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(ftruncate t 1)`,
		Panics: true,
	}).Test(t)
}

func TestFtruncateNoArgs(t *testing.T) {
	(&sliptest.Function{
		Source: `(ftruncate)`,
		Panics: true,
	}).Test(t)
}
