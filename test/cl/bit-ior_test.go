// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestBitIorVectorOk(t *testing.T) {
	(&sliptest.Function{
		Source: "(bit-ior #*1010 #*1100)",
		Expect: "#*1110",
	}).Test(t)
}

func TestBitIorVectorOptArg(t *testing.T) {
	(&sliptest.Function{
		Source: "(bit-ior #*1010 #*1100 #*0000)",
		Expect: "#*1110",
	}).Test(t)
}

func TestBitIorArrayOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(bit-ior
                  (make-array '(2 2) :element-type 'bit :initial-contents (list (list bit1 bit0) (list bit1 bit0)))
                  (make-array '(2 2) :element-type 'bit :initial-contents (list (list bit1 bit1) (list bit0 bit0))))`,
		Array:  true,
		Expect: "#2A((1 1) (1 0))",
	}).Test(t)
	(&sliptest.Function{
		Source: `(bit-ior
                  (make-array '(2 2) :element-type 'bit :initial-contents (list (list bit1 bit0) (list bit1 bit0)))
                  (make-array '(2 2) :element-type 'bit :initial-contents (list (list bit1 bit1) (list bit0 bit0)))
                  (make-array '(2 2) :element-type 'bit :initial-contents (list (list bit0 bit0) (list bit1 bit1))))`,
		Array:  true,
		Expect: "#2A((1 1) (1 0))",
	}).Test(t)
}
