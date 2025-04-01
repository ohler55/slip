// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestSimpleBitVectorPTrue(t *testing.T) {
	(&sliptest.Function{
		Source: "(simple-bit-vector-p #*1010)",
		Expect: "t",
	}).Test(t)
}

func TestSimpleBitVectorPFalse(t *testing.T) {
	(&sliptest.Function{
		Source: "(simple-bit-vector-p (make-array 3 :element-type 'bit :fill-pointer 1))",
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: "(simple-bit-vector-p 7)",
		Expect: "nil",
	}).Test(t)
}
