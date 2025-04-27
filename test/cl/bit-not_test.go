// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestBitNotVectorOk(t *testing.T) {
	(&sliptest.Function{
		Source: "(bit-not #*1010)",
		Expect: "#*0101",
	}).Test(t)
}

func TestBitNotVectorOptArg(t *testing.T) {
	(&sliptest.Function{
		Source: "(bit-not #*1010 #*0000)",
		Expect: "#*0101",
	}).Test(t)
}

func TestBitNotVectorNotBitArray(t *testing.T) {
	(&sliptest.Function{
		Source:    "(bit-not t)",
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestBitNotVectorBadOptArg(t *testing.T) {
	(&sliptest.Function{
		Source:    "(bit-not #*1010 t)",
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    "(bit-not #*1010 #*000)",
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestBitNotArrayOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(bit-not
                  (make-array '(2 2) :element-type 'bit :initial-contents (list (list bit1 bit0) (list bit1 bit0))))`,
		Array:  true,
		Expect: "#2A((0 1) (0 1))",
	}).Test(t)
	(&sliptest.Function{
		Source: `(bit-not
                  (make-array '(2 2) :element-type 'bit :initial-contents (list (list bit1 bit0) (list bit1 bit0)))
                  (make-array '(2 2) :element-type 'bit :initial-contents (list (list bit0 bit0) (list bit1 bit1))))`,
		Array:  true,
		Expect: "#2A((0 1) (0 1))",
	}).Test(t)
}

func TestBitNotArrayNotBitArray(t *testing.T) {
	(&sliptest.Function{
		Source:    `(bit-not (make-array '(2 2) :element-type 'octet))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestBitNotArrayBadOptArg(t *testing.T) {
	(&sliptest.Function{
		Source:    `(bit-not (make-array '(2 2) :element-type 'bit) t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(bit-not (make-array '(2 2) :element-type 'bit) (make-array '(2 3) :element-type 'bit))`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(bit-not (make-array '(2 2) :element-type 'bit) (make-array '(2 2 3) :element-type 'bit))`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}
