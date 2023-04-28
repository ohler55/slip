// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestEqualNumber(t *testing.T) {
	(&sliptest.Function{
		Source: `(equal 3 3)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(equal 3 4)`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: `(equal 3 3.0)`,
		Expect: "t",
	}).Test(t)
}

func TestEqualCharacter(t *testing.T) {
	scope := slip.NewScope()
	scope.Let("a", slip.Character('A'))
	(&sliptest.Function{
		Scope:  scope,
		Source: `(equal #\A a)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(equal #\A #\A)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(equal #\A #\a)`,
		Expect: "nil",
	}).Test(t)
}

func TestEqualString(t *testing.T) {
	(&sliptest.Function{
		Source: `(equal "aBc" "abc")`,
		Expect: "t",
	}).Test(t)
}

func TestEqualCons(t *testing.T) {
	(&sliptest.Function{
		Source: `(equal '(a . 1) '(a . 1))`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(equal '(a . 1) '(a . 2))`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: `(equal '(a . 1) '(b . 1))`,
		Expect: "nil",
	}).Test(t)
}

func TestEqualList(t *testing.T) {
	(&sliptest.Function{
		Source: `(equal '(a (b c)) '(a (b c)))`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(equal '(a (b c)) '(a (b c d)))`,
		Expect: "nil",
	}).Test(t)
}

func TestEqualVector(t *testing.T) {
	(&sliptest.Function{
		Source: `(equal #(a #(b c)) #(a #(b c)))`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(equal #(a (b c)) #(a #(b c)))`,
		Expect: "nil",
	}).Test(t)
}

func TestEqualArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(equal 1)`,
		Panics: true,
	}).Test(t)
}
