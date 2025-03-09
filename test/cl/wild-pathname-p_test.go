// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestWildPathnamePTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(wild-pathname-p "/Users/someone/*.lisp")`,
		Expect: "t",
	}).Test(t)
}

func TestWildPathnamePFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(wild-pathname-p "/Users/someone/quux.lisp")`,
		Expect: "nil",
	}).Test(t)
}

func TestWildPathnamePNotString(t *testing.T) {
	(&sliptest.Function{
		Source:    "(wild-pathname-p 7)",
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
