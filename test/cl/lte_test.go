// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestLteFixnum(t *testing.T) {
	(&sliptest.Function{
		Source: `(<= 3)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(<= 3 4)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(<= 3 3)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(<= 4 3)`,
		Expect: "nil",
	}).Test(t)
}

func TestLteSingleFloat(t *testing.T) {
	(&sliptest.Function{
		Source: `(<= 3.0s+0 4)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(<= 3.0s+0 3)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(<= 3.0s+0 2)`,
		Expect: "nil",
	}).Test(t)
}

func TestLteDoubleFloat(t *testing.T) {
	(&sliptest.Function{
		Source: `(<= 3.0d+0 4)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(<= 3.5s+0 4.5d+0)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(<= 3.5s+0 3.5d+0)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(<= 3.5s+0 3.4d+0)`,
		Expect: "nil",
	}).Test(t)
}

func TestLteLongFloat(t *testing.T) {
	(&sliptest.Function{
		Source: `(<= 2 3.0L+0)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(<= 3.5s+0 3.5L+0)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(<= 3.5s+0 3.4L+0)`,
		Expect: "nil",
	}).Test(t)
}

func TestLteBignum(t *testing.T) {
	(&sliptest.Function{
		Source: `(<= 30000000000000000000 40000000000000000000)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(<= (- 30000000000000000100 30000000000000000000) 100)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(<= (- 30000000000000000100 30000000000000000000) 99)`,
		Expect: "nil",
	}).Test(t)
}

func TestLteRatio(t *testing.T) {
	(&sliptest.Function{
		Source: `(<= 3/4 7/8)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(<= 3/4 0.75)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(<= 3/4 1/2)`,
		Expect: "nil",
	}).Test(t)
}

func TestLteComplex(t *testing.T) {
	(&sliptest.Function{
		Source: `(<= 1 #C(1 2))`,
		Panics: true,
	}).Test(t)
}

func TestLteNotNumber(t *testing.T) {
	(&sliptest.Function{
		Source: `(<= t)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(<= t 1)`,
		Panics: true,
	}).Test(t)
}

func TestLteNoArgs(t *testing.T) {
	(&sliptest.Function{
		Source: `(<=)`,
		Panics: true,
	}).Test(t)
}
