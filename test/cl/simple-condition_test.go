// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/cl"
	"github.com/ohler55/slip/sliptest"
)

func TestSimpleConditionObj(t *testing.T) {
	cond := cl.NewSimpleCondition(nil, "condition ~A-~D", slip.List{slip.Symbol("dummy"), slip.Fixnum(3)})
	(&sliptest.Object{
		Target: cond,
		String: "/^#<simple-condition [0-9a-f]+>$/",
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
	// TBD
	// tt.Equal(t, "condition dummy-3", cond.Error())
}

func TestSimpleConditionMake(t *testing.T) {
	tf := sliptest.Function{
		Source: `(make-condition 'Simple-Condition)`,
		Expect: "/^#<simple-condition [0-9a-f]+>$/",
	}
	tf.Test(t)
	// TBD
	// sc, ok := tf.Result.(cl.SimpleCondition)
	// tt.Equal(t, ok, true)
	// tt.Equal(t, "", sc.Error())

	tf = sliptest.Function{
		Source: `(make-condition 'Simple-Condition :format-control "condition ~A-~D" :format-arguments '(dummy 3))`,
		Expect: "/^#<simple-condition [0-9a-f]+>$/",
	}
	tf.Test(t)
	// TBD
	// sc, ok = tf.Result.(cl.SimpleCondition)
	// tt.Equal(t, ok, true)
	// tt.Equal(t, "condition dummy-3", sc.Error())
	// tt.Equal(t, "condition ~A-~D", sc.Control())
	// tt.Equal(t, "(dummy 3)", slip.ObjectString(sc.Arguments()))
}

func TestSimpleConditionMakeBadArgs(t *testing.T) {
	(&sliptest.Function{
		Source:    `(make-condition 'Simple-Condition :format-control t)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
	(&sliptest.Function{
		Source:    `(make-condition 'Simple-Condition :format-control "~A" :format-arguments t)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}

func TestPanicSimpleCondition(t *testing.T) {
	tt.Panic(t, func() { cl.PanicSimpleCondition(nil, "raise", nil) })
}
