// Copyright (c) 2023, Peter Ohler, All rights reserved.

package test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/cl"
	"github.com/ohler55/slip/sliptest"
)

func TestSeriousConditionObj(t *testing.T) {
	cond := slip.NewSeriousCondition()
	(&sliptest.Object{
		Target: cond,
		String: "/^#<serious-condition [0-9a-f]+>$/",
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
	tt.Equal(t, "/^#<serious-condition [0-9a-f]+>$/", cl.SimpleCondMsg(slip.NewScope(), cond.(slip.Instance)))
}

func TestSeriousConditionMake(t *testing.T) {
	(&sliptest.Function{
		Source: `(make-condition 'serious-condition)`,
		Expect: "/^#<serious-condition [0-9a-f]+>$/",
	}).Test(t)
}

func TestSeriousConditionPanic(t *testing.T) {
	tt.Panic(t, func() { slip.PanicSeriousCondition() })
}
