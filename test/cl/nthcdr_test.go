// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestNthcdrNil(t *testing.T) {
	(&sliptest.Function{
		Source: `(nthcdr 0 nil)`,
		Expect: "nil",
	}).Test(t)
}

func TestNthcdrZero(t *testing.T) {
	(&sliptest.Function{
		Source: `(nthcdr 0 '(a b c))`,
		Expect: "(a b c)",
	}).Test(t)
}

func TestNthcdrInRange(t *testing.T) {
	(&sliptest.Function{
		Source: `(nthcdr 1 '(a b c))`,
		Expect: "(b c)",
	}).Test(t)
}

func TestNthcdrCons(t *testing.T) {
	(&sliptest.Function{
		Source: `(nthcdr 1 '(a . b))`,
		Expect: "b",
	}).Test(t)
}

func TestNthcdrOutOfRange(t *testing.T) {
	(&sliptest.Function{
		Source: `(nthcdr 3 '(a b c))`,
		Expect: "nil",
	}).Test(t)
}

func TestNthcdrNotInt(t *testing.T) {
	(&sliptest.Function{
		Source: `(nthcdr t '(a b c))`,
		Panics: true,
	}).Test(t)
}

func TestNthcdrNotList(t *testing.T) {
	(&sliptest.Function{
		Source: `(nthcdr 1 t)`,
		Panics: true,
	}).Test(t)
}
