// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestFillPointerGet(t *testing.T) {
	(&sliptest.Function{
		Source: `(fill-pointer (make-array 4 :fill-pointer 2))`,
		Expect: "2",
	}).Test(t)
}

func TestFillPointerSetf(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((v (make-array 4 :fill-pointer t :initial-contents '(a b c d))))
                   (setf (fill-pointer v) 2)
                   v)`,
		Array:  true,
		Expect: "#(a b)",
	}).Test(t)
}

func TestFillPointerNotVector(t *testing.T) {
	(&sliptest.Function{
		Source:    `(fill-pointer (make-array '(2 3)))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(setf (fill-pointer (make-array '(2 3))) 3)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	// vector with no fill-pointer
	(&sliptest.Function{
		Source:    `(fill-pointer (make-array 2))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestFillPointerBadFillPointer(t *testing.T) {
	(&sliptest.Function{
		Source:    `(setf (fill-pointer (make-array 2)) t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(setf (fill-pointer (make-array 2)) 3)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(setf (fill-pointer (make-array 2 :fill-pointer 0)) t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
