// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestConstantpNumber(t *testing.T) {
	(&sliptest.Function{
		Source: `(constantp 4)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(constantp 4.1)`,
		Expect: "t",
	}).Test(t)
}

func TestConstantpString(t *testing.T) {
	(&sliptest.Function{
		Source: `(constantp "abc")`,
		Expect: "t",
	}).Test(t)
}

func TestConstantpTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(constantp t)`,
		Expect: "t",
	}).Test(t)
}

func TestConstantpNil(t *testing.T) {
	(&sliptest.Function{
		Source: `(constantp nil)`,
		Expect: "t",
	}).Test(t)
}

func TestConstantpSymbol(t *testing.T) {
	(&sliptest.Function{
		Source: `(constantp :x)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(constantp 'pi)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(constantp 'x)`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: `(let ((x 3)) (constantp x))`,
		Expect: "t",
	}).Test(t)
}

func TestConstantpFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(constantp 'x)`,
		Expect: "nil",
	}).Test(t)
}
