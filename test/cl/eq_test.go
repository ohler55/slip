// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestEqFixnum(t *testing.T) {
	(&sliptest.Function{
		Source: `(eq 3 3)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(eq 3 4)`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: `(eq 3 3.0)`,
		Expect: "nil",
	}).Test(t)
}

func TestEqSymbol(t *testing.T) {
	(&sliptest.Function{
		Source: `(eq 'abc 'abc)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(eq 'abc 'aaa)`,
		Expect: "nil",
	}).Test(t)
}

func TestEqString(t *testing.T) {
	scope := slip.NewScope()
	scope.Let("xyz", slip.String("xyz"))
	(&sliptest.Function{
		Scope:  scope,
		Source: `(eq xyz xyz)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(eq "abc" "abc")`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: `(eq "abc" "aaa")`,
		Expect: "nil",
	}).Test(t)
}

func TestEqPackage(t *testing.T) {
	(&sliptest.Function{
		Source: `(eq *package* *common-lisp-user*)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(eq *package* *common-lisp*)`,
		Expect: "nil",
	}).Test(t)
}
