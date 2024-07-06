// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestSetDifferenceBasic(t *testing.T) {
	(&sliptest.Function{
		Source: `(set-difference '(a b c) '(b d))`,
		Expect: "(a c)",
	}).Test(t)
}

func TestSetDifferenceKey(t *testing.T) {
	(&sliptest.Function{
		Source: `(set-difference '((a . 1) (b . 2) (c . 3)) '((b . 5) (d . 3)) :key 'car)`,
		Expect: "((a . 1) (c . 3))",
	}).Test(t)
}

func TestSetDifferenceTest(t *testing.T) {
	(&sliptest.Function{
		Source: `(set-difference '(#\a #\b #\c) '(#\B #\D) :test 'char-equal)`,
		Expect: `(#\a #\c)`,
	}).Test(t)
}

func TestSetDifferenceNotList(t *testing.T) {
	(&sliptest.Function{
		Source:    `(set-difference t '(b d))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(set-difference '(a b c) t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
