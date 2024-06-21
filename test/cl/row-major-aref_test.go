// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestRowMajorArefVector(t *testing.T) {
	(&sliptest.Function{
		Source: `(row-major-aref (make-array 4 :initial-contents '(a b c d)) 2)`,
		Expect: "c",
	}).Test(t)
}

func TestRowMajorArefVectorSetf(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((v (make-array 4 :initial-contents '(a b c d))))
                   (setf (row-major-aref v 2) 'x)
                   v)`,
		Array:  true,
		Expect: "#(a b x d)",
	}).Test(t)
}

func TestRowMajorArefArray(t *testing.T) {
	(&sliptest.Function{
		Source: `(row-major-aref (make-array '(2 3) :initial-contents '((a b c) (d e f))) 4)`,
		Expect: "e",
	}).Test(t)
}

func TestRowMajorArefArraySetf(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((a (make-array '(2 3) :initial-contents '((a b c) (d e f)))))
                   (setf (row-major-aref a 4) 'x)
                   (row-major-aref a 4))`,
		Expect: "x",
	}).Test(t)
}

func TestRowMajorArefNotArray(t *testing.T) {
	(&sliptest.Function{
		Source:    `(row-major-aref t 2)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(setf (row-major-aref t 2) 3)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestRowMajorArefBadIndex(t *testing.T) {
	(&sliptest.Function{
		Source:    `(row-major-aref (make-array 2) t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(setf (row-major-aref (make-array 2) t) 3)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
