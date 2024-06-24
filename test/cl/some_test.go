// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestSomeTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(some #'characterp "abc")`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(some '< '(1 2 3) '(1 2 4))`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(some '< #(1 2 3) #(1 2 4))`,
		Expect: "t",
	}).Test(t)
}

func TestSomeFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(some '< '(1 2 3) '(1 2))`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: `(some '< '(1 2 3) '(1 2 3))`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: `(some '< #(1 2 3) #(1 2 3))`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: `(some 'char-lessp "def" "ab")`,
		Expect: "nil",
	}).Test(t)
}

func TestSomeBadPredicate(t *testing.T) {
	(&sliptest.Function{
		Source:    `(some t '(1 2 3))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestSomeNotSequence(t *testing.T) {
	(&sliptest.Function{
		Source:    `(some '< t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
