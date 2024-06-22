// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestVectorPushExtendOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((v (make-array 4 :fill-pointer 2 :initial-contents '(a b c d))))
                  (list (vector-push-extend 'x v) v))`,
		Array:  true,
		Expect: "(2 #(a b x))",
	}).Test(t)
}

func TestVectorPushExtendFull(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((v (make-array 4 :fill-pointer t :initial-contents '(a b c d))))
                  (list (vector-push-extend 'x v) v))`,
		Array:  true,
		Expect: "(4 #(a b c d x))",
	}).Test(t)
}

func TestVectorPushExtendNotVector(t *testing.T) {
	(&sliptest.Function{
		Source:    `(vector-push-extend 'x t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestVectorPushExtendNoFillPointer(t *testing.T) {
	(&sliptest.Function{
		Source:    `(vector-push-extend 3 (make-array 4))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
