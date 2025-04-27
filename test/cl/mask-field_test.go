// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestMaskFieldOk(t *testing.T) {
	(&sliptest.Function{
		Source: "(mask-field (byte 3 1) 15)",
		Expect: "14",
	}).Test(t)
}

func TestMaskFieldSetfUnsignedByte(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((x (coerce 15 'unsigned-byte)))
                  (setf (mask-field (byte 2 1) x) 0)
                  x)`,
		Expect: "9",
	}).Test(t)
}

func TestMaskFieldSetfSignedByte(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((x (coerce 15 'signed-byte)))
                  (setf (mask-field (byte 2 1) x) 0)
                  x)`,
		Expect: "9",
	}).Test(t)
}

func TestMaskFieldSetfBignum(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((x (coerce 15 'bignum)))
                  (setf (mask-field (byte 2 1) x) 2)
                  x)`,
		Expect: "13",
	}).Test(t)
}

func TestMaskFieldSetfFixnum(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((x 15))
                  (setf (mask-field (byte 2 1) x) 2)
                  x)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
