// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestLognandFixnum(t *testing.T) {
	(&sliptest.Function{
		Source: `(lognand 7 3)`,
		Expect: "-4",
	}).Test(t)
}

func TestLognandBignum(t *testing.T) {
	(&sliptest.Function{
		Source: `(format nil "~X" (lognand #x33333333333333333333 #x383838383838383838))`,
		Expect: `"ffcfcfcfcfcfcfcfcfcf"`, // same as -303030303030303031
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~X" (lognand #x333333333333333333 #x38383838383838383838))`,
		Expect: `"ffcfcfcfcfcfcfcfcfcf"`, // same as -303030303030303031
	}).Test(t)
}

func TestLognandMixed(t *testing.T) {
	(&sliptest.Function{
		Source: `(format nil "~X" (lognand #x333333333333333333 #x3838383838383838))`,
		Expect: `"ffcfcfcfcfcfcfcfcf"`, // same as -3030303030303031
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~X" (lognand #x3333333333333333 #x383838383838383838))`,
		Expect: `"ffcfcfcfcfcfcfcfcf"`, // same as -3030303030303031
	}).Test(t)
}

func TestLognandNotInteger(t *testing.T) {
	(&sliptest.Function{
		Source:    `(lognand t 1)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(lognand 1 t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(lognand #x1234567890abcdef0 t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
