// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestDepositFieldPositiveFixnum(t *testing.T) {
	(&sliptest.Function{
		Source: "(deposit-field 7 (byte 2 1) 0)",
		Expect: "6",
	}).Test(t)
	(&sliptest.Function{
		Source: "(deposit-field 7 (byte 70 1) 0)",
		Expect: "6",
	}).Test(t)
}

func TestDepositFieldUnsignedByte(t *testing.T) {
	(&sliptest.Function{
		Source: "(deposit-field 7 (byte 2 1) (coerce 0 'unsigned-byte))",
		Expect: "6",
	}).Test(t)
}

func TestDepositFieldSignedByte(t *testing.T) {
	(&sliptest.Function{
		Source: "(deposit-field 7 (byte 2 1) (coerce 0 'signed-byte))",
		Expect: "6",
	}).Test(t)
}

func TestDepositFieldBignum(t *testing.T) {
	(&sliptest.Function{
		Source: "(deposit-field 7 (byte 2 1) (coerce 0 'bignum))",
		Expect: "6",
	}).Test(t)
}

func TestDepositFieldNegativeFixnum(t *testing.T) {
	(&sliptest.Function{
		Source: "(deposit-field 0 (byte 2 1) -3)",
		Expect: "-7",
	}).Test(t)
	(&sliptest.Function{
		Source: "(deposit-field 0 (byte 3 0) -3)",
		Expect: "-8",
	}).Test(t)
	(&sliptest.Function{
		Source: "(deposit-field -1 (byte 4 0) 0)",
		Expect: "15",
	}).Test(t)
	(&sliptest.Function{
		Source: "(deposit-field -1 (byte 64 0) 0)",
		Expect: "18446744073709551615",
	}).Test(t)
	(&sliptest.Function{
		Source: "(deposit-field -1 (byte 67 0) 0)",
		Expect: "147573952589676412927",
	}).Test(t)
}

func TestDepositFieldNegativeBignum(t *testing.T) {
	(&sliptest.Function{
		Source: "(deposit-field 0 (byte 2 1) (coerce -3 'bignum))",
		Expect: "-7",
	}).Test(t)
	(&sliptest.Function{
		Source: "(deposit-field 0 (byte 3 0) (coerce -3 'bignum))",
		Expect: "-8",
	}).Test(t)
	(&sliptest.Function{
		Source: "(deposit-field -1 (byte 4 0) (coerce 0 'bignum))",
		Expect: "15",
	}).Test(t)
}

func TestDepositFieldBadByteSpec(t *testing.T) {
	(&sliptest.Function{
		Source:    "(deposit-field 0 t 0)",
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    "(deposit-field 0 '(t . 7) 0)",
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    "(deposit-field 0 '(7 0) 0)",
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    "(deposit-field 0 '(7 . t) 0)",
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestDepositFieldNotInteger(t *testing.T) {
	(&sliptest.Function{
		Source:    "(deposit-field 0 (byte 2 1) 1.5)",
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
