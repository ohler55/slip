// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestReduceEmpty(t *testing.T) {
	(&sliptest.Function{
		Source: `(reduce #'+ '())`,
		Expect: "nil",
	}).Test(t)
}

func TestReduceOne(t *testing.T) {
	(&sliptest.Function{
		Source: `(reduce #'+ '(1))`,
		Expect: "1",
	}).Test(t)
}

func TestReduceBasic(t *testing.T) {
	(&sliptest.Function{
		Source: `(reduce #'+ '(1 2 3))`,
		Expect: "6",
	}).Test(t)
}

func TestReduceInitialValue(t *testing.T) {
	(&sliptest.Function{
		Source: `(reduce #'+ '(1 2 3) :initial-value 4)`,
		Expect: "10",
	}).Test(t)
}

func TestReduceKey(t *testing.T) {
	(&sliptest.Function{
		Source: `(reduce #'+ '(1 2 3) :key #'1+)`,
		Expect: "9",
	}).Test(t)
}

func TestReduceStartEnd(t *testing.T) {
	(&sliptest.Function{
		Source: `(reduce #'+ '(1 2 3 4 5) :start 1 :end 4)`,
		Expect: "9",
	}).Test(t)
}

func TestReduceFromEnd(t *testing.T) {
	(&sliptest.Function{
		Source: `(reduce #'cons '(1 2 3) :from-end t)`,
		Expect: "(1 2 . 3)",
	}).Test(t)
}

func TestReduceBadStart(t *testing.T) {
	(&sliptest.Function{
		Source:    `(reduce #'+ '(1 2 3 4 5) :start t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestReduceBadEnd(t *testing.T) {
	(&sliptest.Function{
		Source:    `(reduce #'+ '(1 2 3 4 5) :end -1)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
