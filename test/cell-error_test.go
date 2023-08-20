// Copyright (c) 2023, Peter Ohler, All rights reserved.

package test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestCellErrorObj(t *testing.T) {
	cond := slip.NewCellError(slip.Symbol("sell"), "not a %s cell-error", "real")
	(&sliptest.Object{
		Target: cond,
		String: "/^#<CELL-ERROR [0-9a-f]+>$/",
		Simple: func(t2 *testing.T, v any) { _, ok := v.(string); tt.Equal(t2, true, ok) },
		Eval:   cond,
		Equals: []*sliptest.EqTest{
			{Other: cond, Expect: true},
			{Other: slip.True, Expect: false},
		},
	}).Test(t)
	tt.Equal(t, "not a real cell-error", cond.Error())
}

func TestCellErrorMake(t *testing.T) {
	tf := sliptest.Function{
		Source: `(make-condition 'Cell-Error :name :sell)`,
		Expect: "/^#<CELL-ERROR [0-9a-f]+>$/",
	}
	tf.Test(t)
	ce, ok := tf.Result.(slip.CellError)
	tt.Equal(t, ok, true)
	tt.Equal(t, slip.Symbol(":sell"), ce.Name())
	tt.Equal(t, "", ce.Error())

	tf = sliptest.Function{
		Source: `(make-condition 'Cell-Error :name :sell :message "raise")`,
		Expect: "/^#<CELL-ERROR [0-9a-f]+>$/",
	}
	tf.Test(t)
	ce, ok = tf.Result.(slip.CellError)
	tt.Equal(t, ok, true)
	tt.Equal(t, slip.Symbol(":sell"), ce.Name())
	tt.Equal(t, "raise", ce.Error())
}

func TestCellErrorPanic(t *testing.T) {
	tt.Panic(t, func() { slip.PanicCell(slip.Symbol(":sell"), "raise") })
}
