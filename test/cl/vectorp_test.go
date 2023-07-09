// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestVectorpVector(t *testing.T) {
	(&sliptest.Function{
		Source: "(vectorp #(1 2 3))",
		Expect: "t",
	}).Test(t)
}

func TestVectorpString(t *testing.T) {
	(&sliptest.Function{
		Source: `(vectorp "abc")`,
		Expect: "t",
	}).Test(t)
}

func TestVectorpNot(t *testing.T) {
	(&sliptest.Function{
		Source: "(vectorp 7)",
		Expect: "nil",
	}).Test(t)
}
