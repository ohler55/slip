// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestVectorPopOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((v (make-array 4 :fill-pointer 3 :initial-contents '(a b c d))))
                  (list (vector-pop v) v))`,
		Expect: "(c #(a b))",
	}).Test(t)
}

func TestVectorPopEmpty(t *testing.T) {
	(&sliptest.Function{
		Source:    `(vector-pop (make-array 4 :fill-pointer 0 :initial-contents '(a b c d)))`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestVectorPopNotVector(t *testing.T) {
	(&sliptest.Function{
		Source:    `(vector-pop t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestVectorPopNoFillPointer(t *testing.T) {
	(&sliptest.Function{
		Source:    `(vector-pop (make-array 4))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
