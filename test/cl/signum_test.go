// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestSignumFixnum(t *testing.T) {
	(&sliptest.Function{
		Source: `(signum 9)`,
		Expect: "1",
	}).Test(t)
	(&sliptest.Function{
		Source: `(signum -9)`,
		Expect: "-1",
	}).Test(t)
	(&sliptest.Function{
		Source: `(signum 0)`,
		Expect: "0",
	}).Test(t)
}

func TestSignumSingleFloat(t *testing.T) {
	(&sliptest.Function{
		Source: `(signum 9.0s0)`,
		Expect: "1",
	}).Test(t)
	(&sliptest.Function{
		Source: `(signum -9.0s0)`,
		Expect: "-1",
	}).Test(t)
	(&sliptest.Function{
		Source: `(signum 0.0s0)`,
		Expect: "0",
	}).Test(t)
}

func TestSignumDoubleFloat(t *testing.T) {
	(&sliptest.Function{
		Source: `(signum 9.0d0)`,
		Expect: "1",
	}).Test(t)
	(&sliptest.Function{
		Source: `(signum -9.0d0)`,
		Expect: "-1",
	}).Test(t)
	(&sliptest.Function{
		Source: `(signum 0.0d0)`,
		Expect: "0",
	}).Test(t)
}

func TestSignumLongFloat(t *testing.T) {
	(&sliptest.Function{
		Source: `(signum 9.0l0)`,
		Expect: "1",
	}).Test(t)
	(&sliptest.Function{
		Source: `(signum -9.0l0)`,
		Expect: "-1",
	}).Test(t)
	(&sliptest.Function{
		Source: `(signum 0.0l0)`,
		Expect: "0",
	}).Test(t)
}

func TestSignumRatio(t *testing.T) {
	(&sliptest.Function{
		Source: `(signum 9/4)`,
		Expect: "1",
	}).Test(t)
	(&sliptest.Function{
		Source: `(signum -9/4)`,
		Expect: "-1",
	}).Test(t)
	(&sliptest.Function{
		Source: `(signum 0/4)`,
		Expect: "0",
	}).Test(t)
}

func TestSignumBignum(t *testing.T) {
	(&sliptest.Function{
		Source: `(signum 100000000000000000000)`,
		Expect: "1",
	}).Test(t)
	(&sliptest.Function{
		Source: `(signum -100000000000000000000)`,
		Expect: "-1",
	}).Test(t)
}

func TestSignumComplex(t *testing.T) {
	(&sliptest.Function{
		Source: `(signum #C(1 2))`,
		Expect: "#C(1 1)",
	}).Test(t)
	(&sliptest.Function{
		Source: `(signum #C(-1 -2))`,
		Expect: "#C(-1 -1)",
	}).Test(t)
	(&sliptest.Function{
		Source: `(signum #C(0 0))`,
		Expect: "#C(0 0)",
	}).Test(t)
}

func TestSignumBadArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(signum)`,
		Panics: true,
	}).Test(t)
}

func TestSignumNotNumber(t *testing.T) {
	(&sliptest.Function{
		Source: `(signum t)`,
		Panics: true,
	}).Test(t)
}
