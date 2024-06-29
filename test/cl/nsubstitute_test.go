// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestNsubstituteListBasic(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((lst '(a b c)))
                  (list (nsubstitute 2 'b lst) lst))`,
		Expect: "((a 2 c) (a 2 c))",
	}).Test(t)
}

func TestNsubstituteString(t *testing.T) {
	(&sliptest.Function{
		Source: `(nsubstitute #\Q #\q "quux")`,
		Expect: `"Quux"`,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(nsubstitute t #\q "quux")`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestNsubstituteVector(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((vec #(a b c)))
                  (list (nsubstitute 2 'b vec) vec))`,
		Array:  true,
		Expect: "(#(a 2 c) #(a 2 c))",
	}).Test(t)
}

func TestNsubstituteNotSequence(t *testing.T) {
	(&sliptest.Function{
		Source:    `(nsubstitute 2 1 t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
