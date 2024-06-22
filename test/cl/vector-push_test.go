// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestVectorPushOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((v (make-array 4 :fill-pointer 2 :initial-contents '(a b c d))))
                  (list (vector-push 'x v) v))`,
		Array:  true,
		Expect: "(2 #(a b x))",
	}).Test(t)
}

func TestVectorPushFull(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((v (make-array 4 :fill-pointer t :initial-contents '(a b c d))))
                  (list (vector-push 'x v) v))`,
		Array:  true,
		Expect: "(nil #(a b c d))",
	}).Test(t)
}

func TestVectorPushNotVector(t *testing.T) {
	(&sliptest.Function{
		Source:    `(vector-push 'x t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestVectorPushNoFillPointer(t *testing.T) {
	(&sliptest.Function{
		Source:    `(vector-push 3 (make-array 4))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
