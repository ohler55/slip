// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestSimpleVectorPVector(t *testing.T) {
	(&sliptest.Function{
		Source: `(simple-vector-p (make-array 4))`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(simple-vector-p (make-array 4 :fill-pointer 2))`,
		Expect: "nil",
	}).Test(t)
}

func TestSimpleVectorPArray(t *testing.T) {
	(&sliptest.Function{
		Source: `(simple-vector-p (make-array '(2 3)))`,
		Expect: "nil",
	}).Test(t)
}

func TestSimpleVectorPNotArray(t *testing.T) {
	(&sliptest.Function{
		Source: `(simple-vector-p t)`,
		Expect: "nil",
	}).Test(t)
}
