// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestBitOrc2VectorOk(t *testing.T) {
	(&sliptest.Function{
		Source: "(bit-orc2 #*1010 #*1100)",
		Expect: "#*1011",
	}).Test(t)
}

func TestBitOrc2VectorOptArg(t *testing.T) {
	(&sliptest.Function{
		Source: "(bit-orc2 #*1010 #*1100 #*0000)",
		Expect: "#*1011",
	}).Test(t)
}

func TestBitOrc2ArrayOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(bit-orc2
                  (make-array '(2 2) :element-type 'bit :initial-contents (list (list bit1 bit0) (list bit1 bit0)))
                  (make-array '(2 2) :element-type 'bit :initial-contents (list (list bit1 bit1) (list bit0 bit0))))`,
		Array:  true,
		Expect: "#2A((1 0) (1 1))",
	}).Test(t)
	(&sliptest.Function{
		Source: `(bit-orc2
                  (make-array '(2 2) :element-type 'bit :initial-contents (list (list bit1 bit0) (list bit1 bit0)))
                  (make-array '(2 2) :element-type 'bit :initial-contents (list (list bit1 bit1) (list bit0 bit0)))
                  (make-array '(2 2) :element-type 'bit :initial-contents (list (list bit0 bit0) (list bit1 bit1))))`,
		Array:  true,
		Expect: "#2A((1 0) (1 1))",
	}).Test(t)
}
