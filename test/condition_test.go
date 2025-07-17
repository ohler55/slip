// Copyright (c) 2023, Peter Ohler, All rights reserved.

package test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestConditionObj(t *testing.T) {
	cond := slip.NewCondition(nil)
	(&sliptest.Object{
		Target: cond,
		String: "/^#<condition [0-9a-f]+>$/",
		Simple: func(t2 *testing.T, v any) {
			_, ok := v.(map[string]any)
			tt.Equal(t2, true, ok)
		},
		Eval: cond,
		Equals: []*sliptest.EqTest{
			{Other: cond, Expect: true},
			{Other: slip.True, Expect: false},
		},
	}).Test(t)
}

func TestConditionMake(t *testing.T) {
	tf := sliptest.Function{
		Source: `(make-condition 'Condition)`,
		Expect: "/^#<condition [0-9a-f]+>$/",
	}
	tf.Test(t)
}

func TestConditionMakeNotFound(t *testing.T) {
	(&sliptest.Function{
		Source:    `(make-condition 'not-a-condition)`,
		PanicType: slip.Symbol("error"),
	}).Test(t)
}

func TestConditionMakeBadKeyword(t *testing.T) {
	(&sliptest.Function{
		Source:    `(make-condition 'condition 7 7)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}

func TestConditionMakeOddArgs(t *testing.T) {
	(&sliptest.Function{
		Source:    `(make-condition 'condition :bad)`,
		PanicType: slip.Symbol("error"),
	}).Test(t)
}
