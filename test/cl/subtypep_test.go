// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestSubtypepInherit(t *testing.T) {
	(&sliptest.Function{
		Source: "(subtypep 'fixnum 'integer)",
		Expect: "t, t",
	}).Test(t)
	(&sliptest.Function{
		Source: "(subtypep (find-class 'fixnum) 'integer)",
		Expect: "t, t",
	}).Test(t)
}

func TestSubtypepSame(t *testing.T) {
	(&sliptest.Function{
		Source: "(subtypep 'fixnum 'fixnum)",
		Expect: "t, t",
	}).Test(t)
}

func TestSubtypepNot(t *testing.T) {
	(&sliptest.Function{
		Source: "(subtypep 'fixnum 'float)",
		Expect: "nil, t",
	}).Test(t)
}

func TestSubtypepVector(t *testing.T) {
	(&sliptest.Function{
		Source: "(subtypep '(vector fixnum) 'vector)",
		Expect: "t, t",
	}).Test(t)
}

func TestSubtypepNotSymbol(t *testing.T) {
	(&sliptest.Function{
		Source:    "(subtypep 7 'float)",
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    "(subtypep 'float 7)",
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
