// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestSameFixnum(t *testing.T) {
	(&sliptest.Function{
		Source: `(= 3)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(= 3 4)`,
		Expect: "nil",
	}).Test(t)
}

func TestSameSingleFloat(t *testing.T) {
	(&sliptest.Function{
		Source: `(= 3.0s+0)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(= 3.0s+0 3 3.0s+0)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(= 3.0s+0 3.1s+0)`,
		Expect: "nil",
	}).Test(t)
}

func TestSameDoubleFloat(t *testing.T) {
	(&sliptest.Function{
		Source: `(= 3.0d+0)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(= 3 3.0d+0 3.0s+0 3)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(= 3.5s+0 4.5d+0)`,
		Expect: "nil",
	}).Test(t)
}

func TestSameLongFloat(t *testing.T) {
	(&sliptest.Function{
		Source: `(= 3.0L+0)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(= 3 3.0L+0 3.0d+0 3.0s+0)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(= 3.5s+0 4.5L+0)`,
		Expect: "nil",
	}).Test(t)
}

func TestSameBignum(t *testing.T) {
	(&sliptest.Function{
		Source: `(= 30000000000000000000)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(= (- 30000000000000000100 30000000000000000000) 100)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(= 30000000000000000000 20000000000000000000)`,
		Expect: "nil",
	}).Test(t)
}

func TestSameRatio(t *testing.T) {
	(&sliptest.Function{
		Source: `(= 3/4)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(= 3/4 6/8)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(= 3/4 0.75)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(= 3/4 1/2)`,
		Expect: "nil",
	}).Test(t)
}

func TestSameComplex(t *testing.T) {
	(&sliptest.Function{
		Source: `(= #C(1 2))`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(= #C(1 2) #C(1 2))`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(= #C(1 0) 1)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(= #C(1 2) 1)`,
		Expect: "nil",
	}).Test(t)
}

func TestSameNotNumber(t *testing.T) {
	(&sliptest.Function{
		Source: `(= t)`,
		Panics: true,
	}).Test(t)
}

func TestSameNoArgs(t *testing.T) {
	(&sliptest.Function{
		Source: `(=)`,
		Panics: true,
	}).Test(t)
}
