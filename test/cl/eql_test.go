// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestEqlNumber(t *testing.T) {
	(&sliptest.Function{
		Source: `(eql 3 3)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(eql 3 4)`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: `(eql 3 3.0)`,
		Expect: "t",
	}).Test(t)
}

func TestEqlCharacter(t *testing.T) {
	scope := slip.NewScope()
	scope.Let("a", slip.Character('A'))
	(&sliptest.Function{
		Scope:  scope,
		Source: `(eql #\A a)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(eql #\A #\a)`,
		Expect: "nil",
	}).Test(t)
}

func TestEqlArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(eql 3)`,
		Panics: true,
	}).Test(t)
}
