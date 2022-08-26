// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestModFixnum(t *testing.T) {
	(&sliptest.Function{
		Source: `(mod 7 3)`,
		Expect: "1",
	}).Test(t)
	(&sliptest.Function{
		Source: `(mod -1 5)`,
		Expect: "4",
	}).Test(t)
	(&sliptest.Function{
		Source: `(mod 1 -5)`,
		Expect: "-4",
	}).Test(t)
	(&sliptest.Function{
		Source: `(mod -1 -5)`,
		Expect: "-1",
	}).Test(t)
}

func TestModBignum(t *testing.T) {
	(&sliptest.Function{
		Source: `(mod 7 (- 10000000000000000003 10000000000000000000))`,
		Expect: "1",
	}).Test(t)
	(&sliptest.Function{
		Source: `(mod 10000000000000000000 50000000000000000000)`,
		Expect: "10000000000000000000",
	}).Test(t)
	(&sliptest.Function{
		Source: `(mod -10000000000000000000 50000000000000000000)`,
		Expect: "40000000000000000000",
	}).Test(t)
	(&sliptest.Function{
		Source: `(mod 10000000000000000000 -50000000000000000000)`,
		Expect: "-40000000000000000000",
	}).Test(t)
	(&sliptest.Function{
		Source: `(mod -10000000000000000000 -50000000000000000000)`,
		Expect: "-10000000000000000000",
	}).Test(t)
}

func TestModNotReal(t *testing.T) {
	(&sliptest.Function{
		Source: `(mod #C(1 1) 2)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(mod 2 #C(1 1))`,
		Panics: true,
	}).Test(t)
}

func TestModReal(t *testing.T) {
	(&sliptest.Function{
		Source: `(mod 7.5 3)`,
		Expect: "1.5",
	}).Test(t)
	(&sliptest.Function{
		Source: `(mod -1.5 5.0)`,
		Expect: "3.5",
	}).Test(t)
	(&sliptest.Function{
		Source: `(mod 1.5 -5.0)`,
		Expect: "-3.5",
	}).Test(t)
	(&sliptest.Function{
		Source: `(mod -1.5 -5.0)`,
		Expect: "-1.5",
	}).Test(t)
}

func TestModNotNumber(t *testing.T) {
	(&sliptest.Function{
		Source: `(mod t 2)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(mod 2 t)`,
		Panics: true,
	}).Test(t)
}
