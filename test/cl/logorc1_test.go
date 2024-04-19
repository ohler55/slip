// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestLogorc1Fixnum(t *testing.T) {
	(&sliptest.Function{
		Source: `(logorc1 7 3)`,
		Expect: "-5",
	}).Test(t)
}

func TestLogorc1Bignum(t *testing.T) {
	(&sliptest.Function{
		Source: `(format nil "~X" (logorc1 #x33333333333333333333 #x787878787878787878))`,
		Expect: `"ccfcfcfcfcfcfcfcfcfc"`, // same as -33030303030303030304
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~X" (logorc1 #x333333333333333333 #x78787878787878787878))`,
		Expect: `"fffcfcfcfcfcfcfcfcfc"`, // same as -30303030303030304
	}).Test(t)
}

func TestLogorc1Mixed(t *testing.T) {
	(&sliptest.Function{
		Source: `(format nil "~X" (logorc1 #x333333333333333333 #x7878787878787878))`,
		Expect: `"ccfcfcfcfcfcfcfcfc"`, // same as -330303030303030304
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~X" (logorc1 #x3333333333333333 #x787878787878787878))`,
		Expect: `"fffcfcfcfcfcfcfcfc"`, // same as -303030303030304
	}).Test(t)
}

func TestLogorc1NotInteger(t *testing.T) {
	(&sliptest.Function{
		Source:    `(logorc1 t 1)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(logorc1 1 t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(logorc1 #x1234567890abcdef0 t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
