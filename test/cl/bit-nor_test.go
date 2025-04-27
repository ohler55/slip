// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestBitNorVectorOk(t *testing.T) {
	(&sliptest.Function{
		Source: "(bit-nor #*1010 #*1100)",
		Expect: "#*0001",
	}).Test(t)
}

func TestBitNorVectorOptArg(t *testing.T) {
	(&sliptest.Function{
		Source: "(bit-nor #*1010 #*1100 #*0000)",
		Expect: "#*0001",
	}).Test(t)
}

func TestBitNorArrayOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(bit-nor
                  (make-array '(2 2) :element-type 'bit :initial-contents (list (list bit1 bit0) (list bit1 bit0)))
                  (make-array '(2 2) :element-type 'bit :initial-contents (list (list bit1 bit1) (list bit0 bit0))))`,
		Array:  true,
		Expect: "#2A((0 0) (0 1))",
	}).Test(t)
	(&sliptest.Function{
		Source: `(bit-nor
                  (make-array '(2 2) :element-type 'bit :initial-contents (list (list bit1 bit0) (list bit1 bit0)))
                  (make-array '(2 2) :element-type 'bit :initial-contents (list (list bit1 bit1) (list bit0 bit0)))
                  (make-array '(2 2) :element-type 'bit :initial-contents (list (list bit0 bit0) (list bit1 bit1))))`,
		Array:  true,
		Expect: "#2A((0 0) (0 1))",
	}).Test(t)
}
