// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestLdbPositive(t *testing.T) {
	(&sliptest.Function{
		Source: "(ldb (byte 3 1) 15)",
		Expect: "7",
	}).Test(t)
	(&sliptest.Function{
		Source: "(ldb (byte 3 0) 14)",
		Expect: "6",
	}).Test(t)
	(&sliptest.Function{
		Source: "(ldb (byte 68 2) #x70)",
		Expect: "28",
	}).Test(t)
}

func TestLdbNegative(t *testing.T) {
	(&sliptest.Function{
		Source: "(ldb (byte 3 1) -15)",
		Expect: "0",
	}).Test(t)
	(&sliptest.Function{
		Source: "(ldb (byte 4 0) -1)",
		Expect: "15",
	}).Test(t)
}

func TestLdbSetfUnsignedByte(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((x (coerce 15 'unsigned-byte)))
                  (setf (ldb (byte 2 1) x) 0)
                  x)`,
		Expect: "9",
	}).Test(t)
}

func TestLdbSetfSignedByte(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((x (coerce 15 'signed-byte)))
                  (setf (ldb (byte 2 1) x) 0)
                  x)`,
		Expect: "9",
	}).Test(t)
}

func TestLdbSetfBignum(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((x (coerce 15 'bignum)))
                  (setf (ldb (byte 2 1) x) 2)
                  x)`,
		Expect: "13",
	}).Test(t)
}

func TestLdbSetfFixnum(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((x 15))
                  (setf (ldb (byte 2 1) x) 2)
                  x)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
