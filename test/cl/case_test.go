// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestCaseBasic(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((value 3))
                  (case value
                   ((1 2) 'low)
                   (3 'mid)
                   (t 'high)))`,
		Expect: "mid",
	}).Test(t)
	(&sliptest.Function{
		Source: `(case 1
                  ((1 2) 'low)
                  (3 'mid)
                  (t 'high))`,
		Expect: "low",
	}).Test(t)
}

func TestCaseOtherwise(t *testing.T) {
	(&sliptest.Function{
		Source: `(case 5 (1 2) (otherwise 1))`,
		Expect: "1",
	}).Test(t)
}

func TestCaseProgn(t *testing.T) {
	(&sliptest.Function{
		Source: `(case 'c (a 1) ((b c) (+ 2 3)) (t 0))`,
		Expect: "5",
	}).Test(t)
}

func TestCaseEmpty(t *testing.T) {
	(&sliptest.Function{
		Source: `(case 1)`,
		Expect: "nil",
	}).Test(t)
}

func TestCaseOtherwiseOrder(t *testing.T) {
	(&sliptest.Function{
		Source:    `(case 1 (otherwise 2) (3 4))`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestCaseBadClause(t *testing.T) {
	(&sliptest.Function{
		Source:    `(case t t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
