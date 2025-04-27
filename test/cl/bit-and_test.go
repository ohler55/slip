// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestBitAndVectorOk(t *testing.T) {
	(&sliptest.Function{
		Source: "(bit-and #*1010 #*1100)",
		Expect: "#*1000",
	}).Test(t)
}

func TestBitAndVectorOptArg(t *testing.T) {
	(&sliptest.Function{
		Source: "(bit-and #*1010 #*1100 #*0000)",
		Expect: "#*1000",
	}).Test(t)
}

func TestBitAndVectorNotBitArray(t *testing.T) {
	(&sliptest.Function{
		Source:    `(bit-and t #*1010)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(bit-and #*1010 t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(bit-and #*1010 #*0101 t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestBitAndVectorDimDiff(t *testing.T) {
	(&sliptest.Function{
		Source:    `(bit-and #*1010 #*11000)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(bit-and #*1010 #*1100 #*000)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestBitAndArrayOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(bit-and
                  (make-array '(2 2) :element-type 'bit :initial-contents (list (list bit1 bit0) (list bit1 bit0)))
                  (make-array '(2 2) :element-type 'bit :initial-contents (list (list bit1 bit1) (list bit0 bit0))))`,
		Array:  true,
		Expect: "#2A((1 0) (0 0))",
	}).Test(t)
	(&sliptest.Function{
		Source: `(bit-and
                  (make-array '(2 2) :element-type 'bit :initial-contents (list (list bit1 bit0) (list bit1 bit0)))
                  (make-array '(2 2) :element-type 'bit :initial-contents (list (list bit1 bit1) (list bit0 bit0)))
                  (make-array '(2 2) :element-type 'bit :initial-contents (list (list bit0 bit0) (list bit1 bit1))))`,
		Array:  true,
		Expect: "#2A((1 0) (0 0))",
	}).Test(t)
}

func TestBitAndArrayNotBitArray(t *testing.T) {
	(&sliptest.Function{
		Source:    `(bit-and (make-array '(2 2) :element-type 'bit :initial-element 0) t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source: `(bit-and
                     (make-array '(2 2) :element-type 'bit)
                     (make-array '(2 2) :element-type 'bit) t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source: `(bit-and
                     (make-array '(2 2) :element-type 'octet)
                     (make-array '(2 2) :element-type 'bit))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source: `(bit-and
                     (make-array '(2 2) :element-type 'bit)
                     (make-array '(2 2) :element-type 'octet))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source: `(bit-and
                     (make-array '(2 2) :element-type 'bit)
                     (make-array '(2 2) :element-type 'bit)
                     (make-array '(2 2) :element-type 'octet))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestBitAndArrayDimDiff(t *testing.T) {
	(&sliptest.Function{
		Source: `(bit-and
                     (make-array '(2 2) :element-type 'bit)
                     (make-array '(2 3) :element-type 'bit))`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source: `(bit-and
                     (make-array '(2 2) :element-type 'bit)
                     (make-array '(2 2 2) :element-type 'bit))`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source: `(bit-and
                     (make-array '(2 2) :element-type 'bit)
                     (make-array '(2 2) :element-type 'bit)
                     (make-array '(2 3) :element-type 'bit))`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source: `(bit-and
                     (make-array '(2 2) :element-type 'bit)
                     (make-array '(2 2) :element-type 'bit)
                     (make-array '(2 2 3) :element-type 'bit))`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}
