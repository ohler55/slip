// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestFboundpVar(t *testing.T) {
	defer slip.CurrentPackage.Remove("x")
	slip.CurrentPackage.Set("x", nil)
	(&sliptest.Function{
		Source: `(fboundp 'x)`,
		Expect: "nil",
	}).Test(t)
}

func TestFboundpFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(fboundp 'x)`,
		Expect: "nil",
	}).Test(t)
}

func TestFboundpFunc(t *testing.T) {
	(&sliptest.Function{
		Source: `(fboundp 'car)`,
		Expect: "t",
	}).Test(t)
}

func TestFboundpBadArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(fboundp)`,
		Panics: true,
	}).Test(t)
}

func TestFboundpNotSymbol(t *testing.T) {
	(&sliptest.Function{
		Source: `(fboundp 7)`,
		Panics: true,
	}).Test(t)
}
