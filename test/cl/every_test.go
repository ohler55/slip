// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestEveryTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(every #'characterp "abc")`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(every '< '(1 2 3) '(2 3 4))`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(every '< '(1 2 3) '(2 3))`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(every '< #(1 2 3) #(2 3 4))`,
		Expect: "t",
	}).Test(t)
}

func TestEveryFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(every '< '(1 2 3) '(2 3 1))`,
		Expect: "nil",
	}).Test(t)
}

func TestEveryBadPredicate(t *testing.T) {
	(&sliptest.Function{
		Source:    `(every t '(1 2 3))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestEveryNotSequence(t *testing.T) {
	(&sliptest.Function{
		Source:    `(every '< t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
