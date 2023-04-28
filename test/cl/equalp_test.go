// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestEqualpNumber(t *testing.T) {
	(&sliptest.Function{
		Source: `(equalp 3 3)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(equalp 3 4)`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: `(equalp 3 3.0)`,
		Expect: "t",
	}).Test(t)
}

func TestEqualpCharacter(t *testing.T) {
	scope := slip.NewScope()
	scope.Let("a", slip.Character('A'))
	(&sliptest.Function{
		Scope:  scope,
		Source: `(equalp #\A a)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(equalp #\A #\A)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(equalp #\A #\a)`,
		Expect: "nil",
	}).Test(t)
}

func TestEqualpString(t *testing.T) {
	(&sliptest.Function{
		Source: `(equalp "aBc" "abc")`,
		Expect: "t",
	}).Test(t)
}

func TestEqualpCons(t *testing.T) {
	(&sliptest.Function{
		Source: `(equalp '(a . 1) '(a . 1))`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(equalp '(a . 1) '(a . 2))`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: `(equalp '(a . 1) '(b . 1))`,
		Expect: "nil",
	}).Test(t)
}

func TestEqualpList(t *testing.T) {
	(&sliptest.Function{
		Source: `(equalp '(a (b c)) '(a (b c)))`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(equalp '(a (b c)) '(a (b c d)))`,
		Expect: "nil",
	}).Test(t)
}

func TestEqualpVector(t *testing.T) {
	(&sliptest.Function{
		Source: `(equalp #(a #(b c)) #(a #(b c)))`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(equalp #(a (b c)) #(a #(b c)))`,
		Expect: "nil",
	}).Test(t)
}

func TestEqualpArray(t *testing.T) {
	(&sliptest.Function{
		Source: `(equalp #2A((0 1 2 3) (3 2 1 0)) #2A((0 1 2 3) (3 2 1 0)))`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(equalp #2A((0 1 2 3) (3 2 1 0)) #2A((0 1 2 3) (3 2 1 1)))`,
		Expect: "nil",
	}).Test(t)
}

func TestEqualpArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(equalp 1)`,
		Panics: true,
	}).Test(t)
}
