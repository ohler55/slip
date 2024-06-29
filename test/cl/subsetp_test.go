// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestSubsetpBasic(t *testing.T) {
	(&sliptest.Function{
		Source: `(subsetp '(1 3 5) '(1 2 3 4 5))`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(subsetp '(1 3 5) '(1 2 3 4))`,
		Expect: "nil",
	}).Test(t)
}

func TestSubsetpKey(t *testing.T) {
	(&sliptest.Function{
		Source: `(subsetp '((a . 1) (c . 3)) '((a .3) (b . 4) (c . 5)) :key 'car)`,
		Expect: "t",
	}).Test(t)
}

func TestSubsetpTest(t *testing.T) {
	(&sliptest.Function{
		Source: `(subsetp '(1 2 3) '(1 2 4) :test '<)`,
		Expect: "t",
	}).Test(t)
}

func TestSubsetpNotList(t *testing.T) {
	(&sliptest.Function{
		Source:    `(subsetp t '(1 2 3 4 5))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(subsetp '(1 3 5) t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
