// Copyright (c) 2023, Peter Ohler, All rights reserved.

package test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/cl"
	"github.com/ohler55/slip/sliptest"
)

func TestArithmeticErrorObj(t *testing.T) {
	cond := slip.NewArithmeticError(
		slip.Symbol("/"),
		slip.List{slip.Fixnum(1), slip.Fixnum(0)},
		"not a %s arithmetic-error", "real",
	)
	(&sliptest.Object{
		Target: cond,
		String: "/^#<arithmetic-error [0-9a-f]+>$/",
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
	tt.Equal(t, "not a real arithmetic-error", cl.SimpleCondMsg(slip.NewScope(), cond.(slip.Instance)))
}

func TestArithmeticErrorMake(t *testing.T) {
	tf := sliptest.Function{
		Source: `(make-condition 'Arithmetic-Error)`,
		Expect: "/^#<arithmetic-error [0-9a-f]+>$/",
	}
	tf.Test(t)
	cond, ok := tf.Result.(slip.Instance)
	tt.Equal(t, ok, true)
	value, has := cond.SlotValue(slip.Symbol("message"))
	tt.Equal(t, has, true)
	tt.Nil(t, value)
	value, has = cond.SlotValue(slip.Symbol("operation"))
	tt.Equal(t, has, true)
	tt.Nil(t, value)
	value, has = cond.SlotValue(slip.Symbol("operands"))
	tt.Equal(t, has, true)
	tt.Nil(t, value)

	tf = sliptest.Function{
		Source: `(make-condition 'Arithmetic-Error :operation 'divide :operands '(1 0) :message "raise")`,
		Expect: "/^#<arithmetic-error [0-9a-f]+>$/",
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

func TestArithmeticErrorPanic(t *testing.T) {
	tt.Panic(t, func() {
		slip.PanicArithmetic(slip.Symbol("/"), slip.List{slip.Fixnum(1), slip.Fixnum(0)}, "raise")
	})
}
