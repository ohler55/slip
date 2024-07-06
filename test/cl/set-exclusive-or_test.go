// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestSetExclusiveOrBasic(t *testing.T) {
	(&sliptest.Function{
		Source: `(set-exclusive-or '(a b c) '(b d))`,
		Expect: "(a c d)",
	}).Test(t)
}

func TestSetExclusiveOrKey(t *testing.T) {
	(&sliptest.Function{
		Source: `(set-exclusive-or '((a . 1) (b . 2) (c . 3)) '((b . 5) (d . 3)) :key 'car)`,
		Expect: "((a . 1) (c . 3) (d . 3))",
	}).Test(t)
}

func TestSetExclusiveOrTest(t *testing.T) {
	(&sliptest.Function{
		Source: `(set-exclusive-or '(#\a #\b #\c) '(#\B #\D) :test 'char-equal)`,
		Expect: `(#\a #\c #\D)`,
	}).Test(t)
}

func TestSetExclusiveOrNotList(t *testing.T) {
	(&sliptest.Function{
		Source:    `(set-exclusive-or t '(b d))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(set-exclusive-or '(a b c) t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
