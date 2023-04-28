// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestOrTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(or 7)`,
		Expect: "7",
	}).Test(t)
	(&sliptest.Function{
		Source: `(or (+ 1 2) t)`,
		Expect: "3",
	}).Test(t)
	(&sliptest.Function{
		Source: `(or () t)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(or '() t)`,
		Expect: "t",
	}).Test(t)
}

func TestOrFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(or '() nil)`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: `(or nil)`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: `(or)`,
		Expect: "nil",
	}).Test(t)
}

func TestOrBound(t *testing.T) {
	scope := slip.NewScope()
	scope.Let("x", slip.Fixnum(3))
	(&sliptest.Function{
		Scope:  scope,
		Source: `(or x nil)`,
		Expect: "3",
	}).Test(t)
}
