// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestSbitArrayOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(sbit (make-array '(2 3) :element-type 'bit :initial-contents '((1 0 1) (0 1 0))) 0 2)`,
		Expect: "1",
	}).Test(t)
	(&sliptest.Function{
		Source: `(sbit (make-array 2 :element-type 'bit :initial-contents '(1 0)) 1)`,
		Expect: "0",
	}).Test(t)
}

func TestSbitSetf(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((bv (make-array 4 :element-type 'bit :initial-contents '(1 0 1 0))))
                  (setf (aref bv 1) 1)
                  bv)`,
		Array:  true,
		Expect: "#*1110",
	}).Test(t)
	(&sliptest.Function{
		Source: `(let ((ba (make-array '(2 3) :element-type 'bit :initial-contents '((1 0 1) (0 1 0)))))
                  (setf (aref ba 1 0) 1)
                  ba)`,
		Array:  true,
		Expect: "#2A((1 0 1) (1 1 0))",
	}).Test(t)
}

func TestSbitBadSubscript(t *testing.T) {
	(&sliptest.Function{
		Source:    `(sbit (make-array 2 :element-type 'bit :initial-contents '(1 0)) t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(sbit (make-array 2 :element-type 'bit :initial-contents '(1 0)) 3)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(setf (sbit (make-array 2 :element-type 'bit :initial-contents '(1 0)) t) 1)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(setf (sbit (make-array 2 :element-type 'bit :initial-contents '(1 0)) 3) 1)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestSbitNotSimpleBitArray(t *testing.T) {
	(&sliptest.Function{
		Source:    `(sbit t 0 2)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(setf (sbit t 0 2) 3)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(sbit (make-array 2 :element-type 'bit :initial-contents '(1 0) :fill-pointer 0) 0)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(setf (sbit (make-array 2 :element-type 'bit :initial-contents '(1 0) :fill-pointer 0) 0) 0)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
