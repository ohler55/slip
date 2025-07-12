// Copyright (c) 2025, Peter Ohler, All rights reserved.

package test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
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
		String: "/^#<DIVISION-BY-ZERO [0-9a-f]+>$/",
		Simple: func(t2 *testing.T, v any) { _, ok := v.(string); tt.Equal(t2, true, ok) },
		Eval:   cond,
		Equals: []*sliptest.EqTest{
			{Other: cond, Expect: true},
			{Other: slip.True, Expect: false},
		},
	}).Test(t)
	tt.Equal(t, "not a real division-by-zero", cond.Error())
}

func TestDivisionByZeroMake(t *testing.T) {
	tf := sliptest.Function{
		Source: `(make-condition 'Division-By-Zero)`,
		Expect: "/^#<division-by-zero [0-9a-f]+>$/",
	}
	tf.Test(t)
	// TBD
	// ae, ok := tf.Result.(slip.DivisionByZero)
	// tt.Equal(t, ok, true)
	// tt.Equal(t, "/^#<DIVISION-BY-ZERO [0-9a-f]+>$/", ae.Error())
	// tt.Equal(t, "nil", slip.ObjectString(ae.Operation()))
	// tt.Equal(t, "nil", slip.ObjectString(ae.Operands()))

	tf = sliptest.Function{
		Source: `(make-condition 'Division-By-Zero :operation 'divide :operands '(1 0) :message "raise")`,
		Expect: "/^#<division-by-zero [0-9a-f]+>$/",
	}
	tf.Test(t)
	// TBD
	// ae, ok = tf.Result.(slip.DivisionByZero)
	// tt.Equal(t, ok, true)
	// tt.Equal(t, "raise", ae.Error())
	// tt.Equal(t, "divide", slip.ObjectString(ae.Operation()))
	// tt.Equal(t, "(1 0)", slip.ObjectString(ae.Operands()))
}

func TestDivisionByZeroPanic(t *testing.T) {
	tt.Panic(t, func() {
		slip.PanicDivisionByZero(slip.Symbol("/"), slip.List{slip.Fixnum(1), slip.Fixnum(0)}, "raise")
	})
}
