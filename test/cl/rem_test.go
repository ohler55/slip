// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestRemFixnum(t *testing.T) {
	(&sliptest.Function{
		Source: `(rem 7 3)`,
		Expect: "1",
	}).Test(t)
	(&sliptest.Function{
		Source: `(rem -1 5)`,
		Expect: "-1",
	}).Test(t)
	(&sliptest.Function{
		Source: `(rem 1 -5)`,
		Expect: "1",
	}).Test(t)
	(&sliptest.Function{
		Source: `(rem -1 -5)`,
		Expect: "-1",
	}).Test(t)
}

func TestRemBignum(t *testing.T) {
	(&sliptest.Function{
		Source: `(rem 7 (- 10000000000000000003 10000000000000000000))`,
		Expect: "1",
	}).Test(t)
	(&sliptest.Function{
		Source: `(rem 10000000000000000000 50000000000000000000)`,
		Expect: "10000000000000000000",
	}).Test(t)
	(&sliptest.Function{
		Source: `(rem -10000000000000000000 50000000000000000000)`,
		Expect: "-10000000000000000000",
	}).Test(t)
	(&sliptest.Function{
		Source: `(rem 10000000000000000000 -50000000000000000000)`,
		Expect: "10000000000000000000",
	}).Test(t)
	(&sliptest.Function{
		Source: `(rem -10000000000000000000 -50000000000000000000)`,
		Expect: "-10000000000000000000",
	}).Test(t)
}

func TestRemReal(t *testing.T) {
	(&sliptest.Function{
		Source: `(rem 7.5 3)`,
		Expect: "1.5",
	}).Test(t)
	(&sliptest.Function{
		Source: `(rem -1.5 5.0)`,
		Expect: "-1.5",
	}).Test(t)
	(&sliptest.Function{
		Source: `(rem 1.5 -5.0)`,
		Expect: "1.5",
	}).Test(t)
	(&sliptest.Function{
		Source: `(rem -1.5 -5.0)`,
		Expect: "-1.5",
	}).Test(t)
}

func TestRemArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(rem 2)`,
		Panics: true,
	}).Test(t)
}

func TestRemNotReal(t *testing.T) {
	(&sliptest.Function{
		Source: `(rem #C(1 1) 2)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(rem 2 #C(1 1))`,
		Panics: true,
	}).Test(t)
}

func TestRemNotNumber(t *testing.T) {
	(&sliptest.Function{
		Source: `(rem t 2)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(rem 2 t)`,
		Panics: true,
	}).Test(t)
}
