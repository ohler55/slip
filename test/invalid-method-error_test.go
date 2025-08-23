// Copyright (c) 2023, Peter Ohler, All rights reserved.

package test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/cl"
	"github.com/ohler55/slip/sliptest"
)

func TestInvalidMethodErrorObj(t *testing.T) {
	scope := slip.NewScope()
	cond := slip.InvalidMethodErrorNew(
		scope, 0,
		slip.Symbol("vanilla"),
		slip.Symbol(":nonsense"),
		slip.Symbol(":meth"),
		"not a %s invalid-method-error", "real")
	(&sliptest.Object{
		Target: cond,
		String: "/^#<invalid-method-error [0-9a-f]+>$/",
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
	tt.Equal(t, "not a real invalid-method-error", cl.SimpleCondMsg(scope, cond.(slip.Instance)))
}

func TestInvalidMethodErrorMake(t *testing.T) {
	tf := sliptest.Function{
		Source: `(make-condition 'Invalid-Method-Error :class 'vanilla :name :meth :qualifier :nonsense)`,
		Expect: "/^#<invalid-method-error [0-9a-f]+>$/",
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
	tt.Equal(t, slip.Unbound, value)
}

func TestInvalidMethodErrorNoFormat(t *testing.T) {
	cond := slip.InvalidMethodErrorNew(
		slip.NewScope(), 0,
		slip.Symbol("vanilla"),
		slip.Symbol(":nonsense"),
		slip.Symbol(":meth"),
		"")
	tt.Equal(t, ":nonsense :meth is not a valid method combination for vanilla.",
		cl.SimpleCondMsg(slip.NewScope(), cond.(slip.Instance)))
}

func TestInvalidMethodErrorPanic(t *testing.T) {
	tt.Panic(t, func() { slip.InvalidMethodPanic(nil, 0, slip.Symbol("vanilla"), slip.Symbol(":meth"), nil, "raise") })
}
