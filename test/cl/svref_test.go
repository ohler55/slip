// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestSvrefVector(t *testing.T) {
	(&sliptest.Function{
		Source: `(svref (make-array 4 :initial-contents '(a b c d)) 2)`,
		Expect: "c",
	}).Test(t)
}

func TestSvrefVectorSetf(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((v (make-array 4 :initial-contents '(a b c d))))
                   (setf (svref v 2) 'x)
                   v)`,
		Expect: "#(a b x d)",
	}).Test(t)
}

func TestSvrefNotVector(t *testing.T) {
	(&sliptest.Function{
		Source:    `(svref t 2)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(setf (svref t 2) 3)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestSvrefNotSimple(t *testing.T) {
	(&sliptest.Function{
		Source:    `(svref (make-array 4 :fill-pointer t) 2)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(svref (make-array 4 :element-type 'fixnum) 2)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)

	(&sliptest.Function{
		Source:    `(setf (svref (make-array 4 :fill-pointer t) 2) 3)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(setf (svref (make-array 4 :element-type 'fixnum) 2) 3)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestSvrefBadSubscript(t *testing.T) {
	(&sliptest.Function{
		Source:    `(svref (make-array 2) t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(setf (svref (make-array 2) t) 3)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
