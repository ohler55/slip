// Copyright (c) 2023, Peter Ohler, All rights reserved.

package test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/cl"
	"github.com/ohler55/slip/sliptest"
)

func TestTypeErrorObj(t *testing.T) {
	cond := slip.NewTypeError("testing", slip.Fixnum(3), "symbol", "string")
	(&sliptest.Object{
		Target: cond,
		String: "/^#<type-error [0-9a-f]+>$/",
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
	tt.Equal(t, "testing must be a symbol or string not 3, a fixnum.",
		cl.SimpleCondMsg(slip.NewScope(), cond.(slip.Instance)))
}

func TestTypeErrorMake(t *testing.T) {
	tf := sliptest.Function{
		Source: `(make-condition 'Type-Error :datum 3 :expected-type 'symbol)`,
		Expect: "/^#<type-error [0-9a-f]+>$/",
	}
	tf.Test(t)
	cond, ok := tf.Result.(slip.Instance)
	tt.Equal(t, true, ok)
	value, has := cond.SlotValue(slip.Symbol("datum"))
	tt.Equal(t, true, has)
	tt.Equal(t, slip.Fixnum(3), value)
	value, has = cond.SlotValue(slip.Symbol("expected-type"))
	tt.Equal(t, true, has)
	tt.Equal(t, slip.Symbol("symbol"), value)

	tf = sliptest.Function{
		Source: `(make-condition 'Type-Error :datum 3 :expected-type '(symbol "string") )`,
		Expect: "/^#<type-error [0-9a-f]+>$/",
	}
	tf.Test(t)
	cond, ok = tf.Result.(slip.Instance)
	tt.Equal(t, true, ok)
	value, has = cond.SlotValue(slip.Symbol("datum"))
	tt.Equal(t, true, has)
	tt.Equal(t, slip.Fixnum(3), value)
	value, has = cond.SlotValue(slip.Symbol("expected-type"))
	tt.Equal(t, true, has)
	tt.Equal(t, slip.List{slip.Symbol("symbol"), slip.String("string")}, value)
}

func TestTypeErrorPanic(t *testing.T) {
	tt.Panic(t, func() { slip.PanicType("testing", slip.Fixnum(3), "symbol") })
}
