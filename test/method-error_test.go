// Copyright (c) 2023, Peter Ohler, All rights reserved.

package test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/cl"
	"github.com/ohler55/slip/sliptest"
)

func TestMethodErrorObj(t *testing.T) {
	cond := slip.NewMethodError(
		slip.Symbol("vanilla"),
		slip.Symbol(":nonsense"),
		slip.Symbol(":meth"),
		"not a %s method-error", "real")
	(&sliptest.Object{
		Target: cond,
		String: "/^#<method-error [0-9a-f]+>$/",
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
	tt.Equal(t, "not a real method-error", cl.SimpleCondMsg(slip.NewScope(), cond.(slip.Instance)))
}

func TestMethodErrorMake(t *testing.T) {
	tf := sliptest.Function{
		Source: `(make-condition 'Method-Error :class 'vanilla :name :meth :qualifier :nonsense)`,
		Expect: "/^#<method-error [0-9a-f]+>$/",
	}
	tf.Test(t)
	cond, ok := tf.Result.(slip.Instance)
	tt.Equal(t, true, ok)

	value, has := cond.SlotValue(slip.Symbol("name"))
	tt.Equal(t, true, has)
	tt.Equal(t, slip.Symbol(":meth"), value)

	value, has = cond.SlotValue(slip.Symbol("qualifier"))
	tt.Equal(t, true, has)
	tt.Equal(t, slip.Symbol(":nonsense"), value)

	value, has = cond.SlotValue(slip.Symbol("class"))
	tt.Equal(t, true, has)
	tt.Equal(t, slip.Symbol("vanilla"), value)

	value, has = cond.SlotValue(slip.Symbol("message"))
	tt.Equal(t, true, has)
	tt.Nil(t, value)
}

func TestMethodErrorPanic(t *testing.T) {
	tt.Panic(t, func() { slip.PanicMethod(slip.Symbol("vanilla"), slip.Symbol(":meth"), nil, "raise") })
}
