// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestIncfSymbol(t *testing.T) {
	// verify incf returns the modified value
	(&sliptest.Function{
		Source: `(let ((x 0)) (incf x))`,
		Expect: "1",
	}).Test(t)
	// verify x modified
	(&sliptest.Function{
		Source: `(let ((x 0)) (incf x) x)`,
		Expect: "1",
	}).Test(t)
}

func TestIncfPlacer(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((x (list 1 2))) (incf (car x) 2.5) x)`,
		Expect: "(3.5 2)",
	}).Test(t)
}

func TestIncfPlacerNested(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((x (list 1 2))) (incf (car (cdr x)) 2.5) x)`,
		Expect: "(1 4.5)",
	}).Test(t)
}

func TestIncfBadPlacer(t *testing.T) {
	(&sliptest.Function{
		Source: `(incf t 1)`,
		Panics: true,
	}).Test(t)
}
