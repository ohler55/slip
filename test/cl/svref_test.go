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
		Array:  true,
		Expect: "#(a b x d)",
	}).Test(t)
}

func TestSvrefOctets(t *testing.T) {
	(&sliptest.Function{
		Source: `(svref (coerce "abcd" 'octets) 2)`,
		Expect: "99",
	}).Test(t)
}

func TestSvrefOctetsSetf(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((v (coerce "abcd" 'octets)))
                   (setf (svref v 2) (coerce #\x 'octet))
                   v)`,
		Array:  true,
		Expect: "#(97 98 120 100)",
	}).Test(t)
}

func TestSvrefOctetsOutOfBounds(t *testing.T) {
	(&sliptest.Function{
		Source:    `(svref (coerce "abcd" 'octets) 5)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source: `(let ((v (coerce "abcd" 'octets)))
                   (setf (svref v 4) (coerce #\x 'octet))
                   v)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestSvrefOctetsSetfNotOctet(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((v (coerce "abcd" 'octets)))
                   (setf (svref v 2) #\x)
                   v)`,
		PanicType: slip.TypeErrorSymbol,
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
