// Copyright (c) 2023, Peter Ohler, All rights reserved.

package test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestConditionObj(t *testing.T) {
	condition := &slip.ConditionObj{}
	(&sliptest.Object{
		Target: condition,
		String: "/^#<CONDITION [0-9a-f]+>$/",
		Simple: func(t2 *testing.T, v any) { _, ok := v.(string); tt.Equal(t2, true, ok) },
		Eval:   condition,
		Equals: []*sliptest.EqTest{
			{Other: condition, Expect: true},
			{Other: slip.True, Expect: false},
		},
	}).Test(t)
	tt.Equal(t, "/^#<CONDITION [0-9a-f]+>$/", condition.Error())
}

func TestConditionMake(t *testing.T) {
	tf := sliptest.Function{
		Source: `(make-condition 'Condition)`,
		Expect: "/^#<condition [0-9a-f]+>$/",
	}
	tf.Test(t)
	// TBD
	// cond, ok := tf.Result.(slip.Condition)
	// tt.Equal(t, ok, true)
	// co := cond.(*slip.ConditionObj)
	// hier := []slip.Symbol{slip.ErrorSymbol, slip.ConditionSymbol, slip.TrueSymbol}
	// co.SetHierarchy(hier)
	// tt.Equal(t, hier, cond.Hierarchy())
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
