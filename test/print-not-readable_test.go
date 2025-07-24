// Copyright (c) 2025, Peter Ohler, All rights reserved.

package test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/cl"
	"github.com/ohler55/slip/sliptest"
)

func TestPrintNotReadbleObj(t *testing.T) {
	cond := slip.NewPrintNotReadble(slip.Symbol("something"), "not a %s print-not-readable", "real")
	(&sliptest.Object{
		Target: cond,
		String: "/^#<print-not-readable [0-9a-f]+>$/",
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
	tt.Equal(t, "not a real print-not-readable", cl.SimpleCondMsg(slip.NewScope(), cond.(slip.Instance)))
}

func TestPrintNotReadbleMake(t *testing.T) {
	tf := sliptest.Function{
		Source: `(make-condition 'Print-Not-Readable)`,
		Expect: "/^#<print-not-readable [0-9a-f]+>$/",
	}
	tf.Test(t)
	cond, ok := tf.Result.(slip.Instance)
	tt.Equal(t, ok, true)
	value, has := cond.SlotValue(slip.Symbol("object"))
	tt.Equal(t, has, true)
	tt.Nil(t, value)

	tf = sliptest.Function{
		Source: `(make-condition 'Print-Not-Readable :object 'something :message "raise")`,
		Expect: "/^#<print-not-readable [0-9a-f]+>$/",
	}
	tf.Test(t)
	cond, ok = tf.Result.(slip.Instance)
	tt.Equal(t, ok, true)
	value, has = cond.SlotValue(slip.Symbol("object"))
	tt.Equal(t, has, true)
	tt.Equal(t, slip.Symbol("something"), value)
}

func TestPrintNotReadblePanic(t *testing.T) {
	tt.Panic(t, func() {
		slip.PanicPrintNotReadble(slip.Symbol("something"), "raise")
	})
}
