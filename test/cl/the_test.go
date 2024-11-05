// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestTheOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(the list (list 1 2 3))`,
		Expect: "(1 2 3)",
	}).Test(t)
}

func TestTheTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(the t (+ 1 2 3))`,
		Expect: "6",
	}).Test(t)
}

func TestTheSubType(t *testing.T) {
	(&sliptest.Function{
		Source: `(the real (+ 1 2 3))`,
		Expect: "6",
	}).Test(t)
}

func TestTheWrongType(t *testing.T) {
	(&sliptest.Function{
		Source:    `(the list (+ 1 2 3))`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestTheBadValueType(t *testing.T) {
	(&sliptest.Function{
		Source:    `(the 7 (+ 1 2 3))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestTheSetfOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((lst '(1 2 3))) (setf (the fixnum (car lst)) 4) lst)`,
		Expect: "(4 2 3)",
	}).Test(t)
}
