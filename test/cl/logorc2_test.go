// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestLogorc2Fixnum(t *testing.T) {
	(&sliptest.Function{
		Source: `(logorc2 3 7)`,
		Expect: "-5",
	}).Test(t)
}

func TestLogorc2Bignum(t *testing.T) {
	(&sliptest.Function{
		Source: `(format nil "~X" (logorc2 #x787878787878787878 #x33333333333333333333))`,
		Expect: `"ccfcfcfcfcfcfcfcfcfc"`, // same as -33030303030303030304
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~X" (logorc2 #x78787878787878787878 #x333333333333333333))`,
		Expect: `"fffcfcfcfcfcfcfcfcfc"`, // same as -30303030303030304
	}).Test(t)
}

func TestLogorc2Mixed(t *testing.T) {
	(&sliptest.Function{
		Source: `(format nil "~X" (logorc2 #x7878787878787878 #x333333333333333333))`,
		Expect: `"ccfcfcfcfcfcfcfcfc"`, // same as -330303030303030304
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~X" (logorc2 #x787878787878787878 #x3333333333333333))`,
		Expect: `"fffcfcfcfcfcfcfcfc"`, // same as -303030303030304
	}).Test(t)
}

func TestLogorc2NotInteger(t *testing.T) {
	(&sliptest.Function{
		Source:    `(logorc2 t 1)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(logorc2 1 t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(logorc2 #x1234567890abcdef0 t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
