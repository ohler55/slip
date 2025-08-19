// Copyright (c) 2025, Peter Ohler, All rights reserved.

package test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/cl"
	"github.com/ohler55/slip/sliptest"
)

func TestDivisionByZeroObj(t *testing.T) {
	cond := slip.NewDivisionByZero(
		slip.Symbol("/"),
		slip.List{slip.Fixnum(1), slip.Fixnum(0)},
		"not a %s division-by-zero", "real",
	)
	(&sliptest.Object{
		Target: cond,
		String: "/^#<division-by-zero [0-9a-f]+>$/",
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
	tt.Equal(t, "not a real division-by-zero", cl.SimpleCondMsg(slip.NewScope(), cond.(slip.Instance)))
}

func TestDivisionByZeroMake(t *testing.T) {
	tf := sliptest.Function{
		Source: `(make-condition 'Division-By-Zero)`,
		Expect: "/^#<division-by-zero [0-9a-f]+>$/",
	}
	tf.Test(t)
	cond, ok := tf.Result.(slip.Instance)
	tt.Equal(t, ok, true)
	value, has := cond.SlotValue(slip.Symbol("message"))
	tt.Equal(t, has, true)
	tt.Equal(t, slip.Unbound, value)
	value, has = cond.SlotValue(slip.Symbol("operation"))
	tt.Equal(t, has, true)
	tt.Equal(t, slip.Unbound, value)
	value, has = cond.SlotValue(slip.Symbol("operands"))
	tt.Equal(t, has, true)
	tt.Equal(t, slip.Unbound, value)

	tf = sliptest.Function{
		Source: `(make-condition 'Division-By-Zero :operation 'divide :operands '(1 0) :message "raise")`,
		Expect: "/^#<division-by-zero [0-9a-f]+>$/",
	}
	tf.Test(t)
	cond, ok = tf.Result.(slip.Instance)
	tt.Equal(t, ok, true)

	value, has = cond.SlotValue(slip.Symbol("message"))
	tt.Equal(t, has, true)
	tt.Equal(t, slip.String("raise"), value)

	value, has = cond.SlotValue(slip.Symbol("operation"))
	tt.Equal(t, has, true)
	tt.Equal(t, slip.Symbol("divide"), value)

	value, has = cond.SlotValue(slip.Symbol("operands"))
	tt.Equal(t, has, true)
	tt.Equal(t, slip.List{slip.Fixnum(1), slip.Fixnum(0)}, value)
}

func TestDivisionByZeroPanic(t *testing.T) {
	tt.Panic(t, func() {
		slip.PanicDivisionByZero(slip.Symbol("/"), slip.List{slip.Fixnum(1), slip.Fixnum(0)}, "raise")
	})
}
