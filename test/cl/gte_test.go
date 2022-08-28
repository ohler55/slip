// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestGteFixnum(t *testing.T) {
	(&sliptest.Function{
		Source: `(>= 3)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(>= 4 3)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(>= 3 3)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(>= 3 4)`,
		Expect: "nil",
	}).Test(t)
}

func TestGteSingleFloat(t *testing.T) {
	(&sliptest.Function{
		Source: `(>= 3.0s+0 2)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(>= 3.0s+0 3)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(>= 3.0s+0 4)`,
		Expect: "nil",
	}).Test(t)
}

func TestGteDoubleFloat(t *testing.T) {
	(&sliptest.Function{
		Source: `(>= 3.0d+0 2)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(>= 3.5s+0 2.5d+0)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(>= 3.5s+0 3.5d+0)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(>= 3.5s+0 3.6d+0)`,
		Expect: "nil",
	}).Test(t)
}

func TestGteLongFloat(t *testing.T) {
	(&sliptest.Function{
		Source: `(>= 4 3.0L+0)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(>= 3.5s+0 3.5L+0)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(>= 3.5s+0 3.6L+0)`,
		Expect: "nil",
	}).Test(t)
}

func TestGteBignum(t *testing.T) {
	(&sliptest.Function{
		Source: `(>= 30000000000000000000 20000000000000000000)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(>= (- 30000000000000000100 30000000000000000000) 100)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(>= (- 30000000000000000100 30000000000000000000) 101)`,
		Expect: "nil",
	}).Test(t)
}

func TestGteRatio(t *testing.T) {
	(&sliptest.Function{
		Source: `(>= 3/4 5/8)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(>= 3/4 0.75)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(>= 3/4 1/2)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(>= 3/4 7/8)`,
		Expect: "nil",
	}).Test(t)
}

func TestGteComplex(t *testing.T) {
	(&sliptest.Function{
		Source: `(>= 1 #C(1 2))`,
		Panics: true,
	}).Test(t)
}

func TestGteNotNumber(t *testing.T) {
	(&sliptest.Function{
		Source: `(>= t)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(>= t 1)`,
		Panics: true,
	}).Test(t)
}

func TestGteNoArgs(t *testing.T) {
	(&sliptest.Function{
		Source: `(>=)`,
		Panics: true,
	}).Test(t)
}
