// Copyright (c) 2023, Peter Ohler, All rights reserved.

package test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/cl"
	"github.com/ohler55/slip/sliptest"
)

func TestControlErrorObj(t *testing.T) {
	cond := slip.NewControlError("not a %s control-error", "real")
	(&sliptest.Object{
		Target: cond,
		String: "/^#<control-error [0-9a-f]+>$/",
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
	tt.Equal(t, "not a real control-error", cl.SimpleCondMsg(slip.NewScope(), cond.(slip.Instance)))
}

func TestControlErrorMake(t *testing.T) {
	tf := sliptest.Function{
		Source: `(make-condition 'Control-Error)`,
		Expect: "/^#<control-error [0-9a-f]+>$/",
	}
	tf.Test(t)
	cond, ok := tf.Result.(slip.Instance)
	tt.Equal(t, true, ok)
	value, has := cond.SlotValue(slip.Symbol("message"))
	tt.Equal(t, true, has)
	tt.Nil(t, value)

	tf = sliptest.Function{
		Source: `(make-condition 'Control-Error :message "raise")`,
		Expect: "/^#<control-error [0-9a-f]+>$/",
	}
	tf.Test(t)
	cond, ok = tf.Result.(slip.Instance)
	tt.Equal(t, true, ok)
	value, has = cond.SlotValue(slip.Symbol("message"))
	tt.Equal(t, true, has)
	tt.Equal(t, slip.String("raise"), value)
}

func TestControlErrorPanic(t *testing.T) {
	tt.Panic(t, func() {
		slip.PanicControl("raise")
	})
}
