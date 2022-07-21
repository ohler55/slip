// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestAtomTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(atom 7)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(atom '())`,
		Expect: "t",
	}).Test(t)
}

func TestAtomFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(atom '(1 2))`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: `(atom '(1 . 2))`,
		Expect: "nil",
	}).Test(t)
}

func TestAtomBadArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(atom)`,
		Panics: true,
	}).Test(t)
}
