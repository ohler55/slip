// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestLognorFixnum(t *testing.T) {
	(&sliptest.Function{
		Source: `(lognor 7 3)`,
		Expect: "-8",
	}).Test(t)
}

func TestLognorBignum(t *testing.T) {
	(&sliptest.Function{
		Source: `(format nil "~X" (lognor #x33333333333333333333 #x383838383838383838))`,
		Expect: `"ccc4c4c4c4c4c4c4c4c4"`, // same as -333B3B3B3B3B3B3B3B3C
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~X" (lognor #x333333333333333333 #x38383838383838383838))`,
		Expect: `"c7c4c4c4c4c4c4c4c4c4"`, // same as -383B3B3B3B3B3B3B3B3C
	}).Test(t)
}

func TestLognorMixed(t *testing.T) {
	(&sliptest.Function{
		Source: `(format nil "~X" (lognor #x333333333333333333 #x3838383838383838))`,
		Expect: `"ccc4c4c4c4c4c4c4c4"`, // same as -333B3B3B3B3B3B3B3C
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~X" (lognor #x3333333333333333 #x383838383838383838))`,
		Expect: `"c7c4c4c4c4c4c4c4c4"`, // same as -383B3B3B3B3B3B3B3C
	}).Test(t)
}

func TestLognorNotInteger(t *testing.T) {
	(&sliptest.Function{
		Source:    `(lognor t 1)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(lognor 1 t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(lognor #x1234567890abcdef0 t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
