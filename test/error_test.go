// Copyright (c) 2023, Peter Ohler, All rights reserved.

package test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/cl"
	"github.com/ohler55/slip/sliptest"
)

func TestErrorObj(t *testing.T) {
	err := slip.NewError("not a %s error", "real")
	(&sliptest.Object{
		Target: err,
		String: "/^#<error [0-9a-f]+>$/",
		Simple: func(t2 *testing.T, v any) {
			_, ok := v.(map[string]any)
			tt.Equal(t2, true, ok)
		},
		Eval: err,
		Equals: []*sliptest.EqTest{
			{Other: err, Expect: true},
			{Other: slip.True, Expect: false},
		},
	}).Test(t)
	tt.Equal(t, "not a real error", cl.SimpleCondMsg(slip.NewScope(), err.(slip.Instance)))
}

func TestErrorMake(t *testing.T) {
	tf := sliptest.Function{
		Source: `(make-condition 'Error)`,
		Expect: "/^#<error [0-9a-f]+>$/",
	}
	tf.Test(t)
	cond, ok := tf.Result.(slip.Instance)
	tt.Equal(t, true, ok)
	value, has := cond.SlotValue(slip.Symbol("message"))
	tt.Equal(t, true, has)
	tt.Nil(t, value)

	tf = sliptest.Function{
		Source: `(make-condition 'Error :message "raise")`,
		Expect: "/^#<error [0-9a-f]+>$/",
	}
	tf.Test(t)
	cond, ok = tf.Result.(slip.Instance)
	tt.Equal(t, true, ok)
	value, has = cond.SlotValue(slip.Symbol("message"))
	tt.Equal(t, true, has)
	tt.Equal(t, slip.String("raise"), value)
}

func TestErrorPanic(t *testing.T) {
	tt.Panic(t, func() { slip.PanicError("raise") })
}
