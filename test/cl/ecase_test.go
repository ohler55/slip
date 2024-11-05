// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestEcaseBasic(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((value 3))
                  (ecase value
                   ((1 2) 'low)
                   (3 'mid)
                   (4 'high)))`,
		Expect: "mid",
	}).Test(t)
	(&sliptest.Function{
		Source: `(ecase 1
                  ((1 2) 'low)
                  (3 'mid)
                  (4 'high))`,
		Expect: "low",
	}).Test(t)
}

func TestEcaseNoMatch(t *testing.T) {
	(&sliptest.Function{
		Source:    `(ecase 5 (1 'one) ((2 3) 'several))`,
		PanicType: slip.TypeErrorSymbol,
		Validate: func(t *testing.T, v slip.Object) {
			te, ok := v.(slip.TypeError)
			tt.Equal(t, true, ok)
			str := te.Error()
			tt.Equal(t, "/1, 2 or 3/", str)
		},
	}).Test(t)
}

func TestEcaseProgn(t *testing.T) {
	(&sliptest.Function{
		Source: `(ecase 'c (a 1) ((b c) (+ 2 3)))`,
		Expect: "5",
	}).Test(t)
}

func TestEcaseBadClause(t *testing.T) {
	(&sliptest.Function{
		Source:    `(ecase t t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
