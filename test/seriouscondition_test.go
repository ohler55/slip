// Copyright (c) 2023, Peter Ohler, All rights reserved.

package test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestSeriousConditionObj(t *testing.T) {
	condition := slip.MakeCondition("serious-condition", nil)
	(&sliptest.Object{
		Target: condition,
		String: "/^#<SERIOUS-CONDITION [0-9a-f]+>$/",
		Simple: func(t2 *testing.T, v any) { _, ok := v.(string); tt.Equal(t2, true, ok) },
		Eval:   condition,
		Equals: []*sliptest.EqTest{
			{Other: condition, Expect: true},
			{Other: slip.True, Expect: false},
		},
	}).Test(t)
	tt.Equal(t, "/^#<SERIOUS-CONDITION [0-9a-f]+>$/", condition.Error())
}

func TestSeriousConditionMake(t *testing.T) {
	(&sliptest.Function{
		Source: `(make-condition 'serious-condition)`,
		Expect: "/^#<serious-condition [0-9a-f]+>$/",
	}).Test(t)
}
