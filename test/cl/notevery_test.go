// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestNoteveryTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(notevery '< '(1 2 3) '(2 2 4))`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(notevery '< #(1 2 3) #(2 2 4))`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(notevery '< (coerce #(1 2 3) 'octets) (coerce #(2 2 4) 'octets))`,
		Expect: "t",
	}).Test(t)
}

func TestNoteveryFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(notevery #'characterp "abc")`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: `(notevery '< '(1 2 3) '(2 3))`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: `(notevery '< '(1 2 3) '(2 3 4))`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: `(notevery '< #(1 2 3) #(2 3))`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: `(notevery '< (coerce #(1 2 3) 'octets) (coerce #(2 3) 'octets))`,
		Expect: "nil",
	}).Test(t)
}

func TestNoteveryBadPredicate(t *testing.T) {
	(&sliptest.Function{
		Source:    `(notevery t '(1 2 3))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestNoteveryNotSequence(t *testing.T) {
	(&sliptest.Function{
		Source:    `(notevery '< t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
