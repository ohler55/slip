// Copyright (c) 2023, Peter Ohler, All rights reserved.

package test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
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
		String: "/^#<ARITHMETIC-ERROR [0-9a-f]+>$/",
		Simple: func(t2 *testing.T, v any) { _, ok := v.(string); tt.Equal(t2, true, ok) },
		Eval:   cond,
		Equals: []*sliptest.EqTest{
			{Other: cond, Expect: true},
			{Other: slip.True, Expect: false},
		},
	}).Test(t)
	tt.Equal(t, "not a real arithmetic-error", cond.Error())
}

func TestArithmeticErrorMake(t *testing.T) {
	tf := sliptest.Function{
		Source: `(make-condition 'Arithmetic-Error)`,
		Expect: "/^#<ARITHMETIC-ERROR [0-9a-f]+>$/",
	}
	tf.Test(t)
	ae, ok := tf.Result.(slip.ArithmeticError)
	tt.Equal(t, ok, true)
	tt.Equal(t, "/^#<ARITHMETIC-ERROR [0-9a-f]+>$/", ae.Error())
	tt.Equal(t, "nil", slip.ObjectString(ae.Operation()))
	tt.Equal(t, "nil", slip.ObjectString(ae.Operands()))

	tf = sliptest.Function{
		Source: `(make-condition 'Arithmetic-Error :operation 'divide :operands '(1 0) :message "raise")`,
		Expect: "/^#<ARITHMETIC-ERROR [0-9a-f]+>$/",
	}
	tf.Test(t)
	ae, ok = tf.Result.(slip.ArithmeticError)
	tt.Equal(t, ok, true)
	tt.Equal(t, "raise", ae.Error())
	tt.Equal(t, "divide", slip.ObjectString(ae.Operation()))
	tt.Equal(t, "(1 0)", slip.ObjectString(ae.Operands()))
}

func TestArithmeticErrorPanic(t *testing.T) {
	tt.Panic(t, func() {
		slip.PanicArithmetic(slip.Symbol("/"), slip.List{slip.Fixnum(1), slip.Fixnum(0)}, "raise")
	})
}
