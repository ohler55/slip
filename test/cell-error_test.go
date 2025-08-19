// Copyright (c) 2023, Peter Ohler, All rights reserved.

package test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/cl"
	"github.com/ohler55/slip/sliptest"
)

func TestCellErrorObj(t *testing.T) {
	cond := slip.NewCellError(slip.Symbol("sell"), "not a %s cell-error", "real")
	(&sliptest.Object{
		Target: cond,
		String: "/^#<cell-error [0-9a-f]+>$/",
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
	tt.Equal(t, "not a real cell-error", cl.SimpleCondMsg(slip.NewScope(), cond.(slip.Instance)))
}

func TestCellErrorMake(t *testing.T) {
	tf := sliptest.Function{
		Source: `(make-condition 'Cell-Error :name :sell)`,
		Expect: "/^#<cell-error [0-9a-f]+>$/",
	}
	tf.Test(t)
	cond, ok := tf.Result.(slip.Instance)
	tt.Equal(t, true, ok)
	value, has := cond.SlotValue(slip.Symbol("name"))
	tt.Equal(t, true, has)
	tt.Equal(t, slip.Symbol(":sell"), value)
	value, has = cond.SlotValue(slip.Symbol("message"))
	tt.Equal(t, true, has)
	tt.Equal(t, slip.Unbound, value)

	tf = sliptest.Function{
		Source: `(make-condition 'Cell-Error :name :sell :message "raise")`,
		Expect: "/^#<cell-error [0-9a-f]+>$/",
	}
	tf.Test(t)
	cond, ok = tf.Result.(slip.Instance)
	tt.Equal(t, true, ok)
	value, has = cond.SlotValue(slip.Symbol("name"))
	tt.Equal(t, true, has)
	tt.Equal(t, slip.Symbol(":sell"), value)
	value, has = cond.SlotValue(slip.Symbol("message"))
	tt.Equal(t, true, has)
	tt.Equal(t, slip.String("raise"), value)
}

func TestCellErrorPanic(t *testing.T) {
	tt.Panic(t, func() { slip.PanicCell(slip.Symbol(":sell"), "raise") })
}
