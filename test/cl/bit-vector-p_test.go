// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestBitVectorPTrue(t *testing.T) {
	(&sliptest.Function{
		Source: "(bit-vector-p #*1010)",
		Expect: "t",
	}).Test(t)
}

func TestBitVectorPFalse(t *testing.T) {
	(&sliptest.Function{
		Source: "(bit-vector-p 7)",
		Expect: "nil",
	}).Test(t)
}
