// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestBoundpVar(t *testing.T) {
	defer slip.CurrentPackage.Remove("x")
	slip.CurrentPackage.Set("x", nil)
	(&sliptest.Function{
		Source: `(boundp 'x)`,
		Expect: "t",
	}).Test(t)
}

func TestBoundpFunc(t *testing.T) {
	(&sliptest.Function{
		Source: `(boundp 'car)`,
		Expect: "nil",
	}).Test(t)
}

func TestBoundpLet(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((z 3)) (boundp 'z))`,
		Expect: "t",
	}).Test(t)
}

func TestBoundpFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(boundp 'y)`,
		Expect: "nil",
	}).Test(t)
}

func TestBoundpBadArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(boundp)`,
		Panics: true,
	}).Test(t)
}

func TestBoundpNotSymbol(t *testing.T) {
	(&sliptest.Function{
		Source: `(boundp 7)`,
		Panics: true,
	}).Test(t)
}
