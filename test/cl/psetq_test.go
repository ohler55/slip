// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestPsetqBasic(t *testing.T) {
	scope := slip.NewScope()
	(&sliptest.Function{
		Scope:  scope,
		Source: `(psetq x 7)`,
		Expect: "7",
	}).Test(t)
	tt.Equal(t, slip.Fixnum(7), scope.Get(slip.Symbol("x")))
}

func TestPsetqEmpty(t *testing.T) {
	(&sliptest.Function{
		Source: `(psetq)`,
		Expect: "nil",
	}).Test(t)
}

func TestPsetqMultiple(t *testing.T) {
	scope := slip.NewScope()
	scope.Set(slip.Symbol("x"), slip.Fixnum(5))
	(&sliptest.Function{
		Scope:  scope,
		Source: `(psetq x 7 y (1+ x))`,
		Expect: "6",
	}).Test(t)
	tt.Equal(t, slip.Fixnum(7), scope.Get(slip.Symbol("x")))
	tt.Equal(t, slip.Fixnum(6), scope.Get(slip.Symbol("y")))
}

func TestPsetqBadArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(psetq x)`,
		Panics: true,
	}).Test(t)
}

func TestPsetqBadSymbol(t *testing.T) {
	(&sliptest.Function{
		Source: `(psetq t 1)`,
		Panics: true,
	}).Test(t)
}
