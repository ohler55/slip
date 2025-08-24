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
	cond := cl.SimpleConditionNew(nil, 0, "condition ~A-~D", slip.List{slip.Symbol("dummy"), slip.Fixnum(3)})
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
	tt.Equal(t, "condition dummy-3", cl.SimpleCondMsg(slip.NewScope(), cond.(slip.Instance)))
}

func TestSimpleConditionMake(t *testing.T) {
	tf := sliptest.Function{
		Source: `(make-condition 'Simple-Condition :format-control "condition ~A-~D" :format-arguments '(dummy 3))`,
		Expect: "/^#<simple-condition [0-9a-f]+>$/",
	}
	tf.Test(t)
	cond, ok := tf.Result.(slip.Instance)
	tt.Equal(t, true, ok)
	value, has := cond.SlotValue(slip.Symbol("format-control"))
	tt.Equal(t, true, has)
	tt.Equal(t, slip.String("condition ~A-~D"), value)
	value, has = cond.SlotValue(slip.Symbol("format-arguments"))
	tt.Equal(t, true, has)
	tt.Equal(t, slip.List{slip.Symbol("dummy"), slip.Fixnum(3)}, value)
}

func TestSimpleConditionEmpty(t *testing.T) {
	tf := sliptest.Function{
		Source: `(make-condition 'Simple-Condition)`,
		Expect: "/^#<simple-condition [0-9a-f]+>$/",
	}
	tf.Test(t)
	cond, ok := tf.Result.(slip.Instance)
	tt.Equal(t, true, ok)
	tt.Equal(t, "/^#<simple-condition [0-9a-f]+>$/", cl.SimpleCondMsg(slip.NewScope(), cond))
}

func TestSimpleConditionMessage(t *testing.T) {
	tf := sliptest.Function{
		Source: `(make-condition 'Warning :message "raise")`,
		Expect: "/^#<warning [0-9a-f]+>$/",
	}
	tf.Test(t)
	cond, ok := tf.Result.(slip.Instance)
	tt.Equal(t, true, ok)
	tt.Equal(t, "raise", cl.SimpleCondMsg(slip.NewScope(), cond))

	tf = sliptest.Function{
		Source: `(make-condition 'Warning :message 'raise)`,
		Expect: "/^#<warning [0-9a-f]+>$/",
	}
	tf.Test(t)
	cond, ok = tf.Result.(slip.Instance)
	tt.Equal(t, true, ok)
	tt.Equal(t, "/^#<warning [0-9a-f]+>$/", cl.SimpleCondMsg(slip.NewScope(), cond))
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
	tt.Panic(t, func() { cl.SimpleConditionPanic(nil, 0, "raise", nil) })
}
