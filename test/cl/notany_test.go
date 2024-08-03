// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestNotanyTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(notany '< '(2 3 4) '(1 2 3))`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(notany '< '(2 3) '(1 2 3))`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(notany '< #(2 3 4) #(1 2 3))`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(notany #'numberp "abc")`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(notany #'evenp (coerce "ace" 'octets))`,
		Expect: "t",
	}).Test(t)
}

func TestNotanyFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(notany #'characterp "abc")`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: `(notany '< '(1 2 3) '(2 3 1))`,
		Expect: "nil",
	}).Test(t)
}

func TestNotanyBadPredicate(t *testing.T) {
	(&sliptest.Function{
		Source:    `(notany t '(1 2 3))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestNotanyNotSequence(t *testing.T) {
	(&sliptest.Function{
		Source:    `(notany '< t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
