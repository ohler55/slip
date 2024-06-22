// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestArefVector(t *testing.T) {
	(&sliptest.Function{
		Source: `(aref (make-array 4 :initial-contents '(a b c d)) 2)`,
		Expect: "c",
	}).Test(t)
}

func TestArefVectorSetf(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((v (make-array 4 :initial-contents '(a b c d))))
                   (setf (aref v 2) 'x)
                   v)`,
		Array:  true,
		Expect: "#(a b x d)",
	}).Test(t)
}

func TestArefArray(t *testing.T) {
	(&sliptest.Function{
		Source: `(aref (make-array '(2 3) :initial-contents '((a b c) (d e f))) 0 2)`,
		Expect: "c",
	}).Test(t)
}

func TestArefArraySetf(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((a (make-array '(2 3) :initial-contents '((a b c) (d e f)))))
                   (setf (aref a 0 2) 'x)
                   (aref a 0 2))`,
		Expect: "x",
	}).Test(t)
}

func TestArefNotArray(t *testing.T) {
	(&sliptest.Function{
		Source:    `(aref t 0 2)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(setf (aref t 0 2) 3)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestArefBadSubscript(t *testing.T) {
	(&sliptest.Function{
		Source:    `(aref (make-array 2) t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(setf (aref (make-array 2) t) 3)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
