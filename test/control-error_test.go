// Copyright (c) 2023, Peter Ohler, All rights reserved.

package test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestControlErrorObj(t *testing.T) {
	cond := slip.NewControlError("not a %s control-error", "real")
	(&sliptest.Object{
		Target: cond,
		String: "/^#<CONTROL-ERROR [0-9a-f]+>$/",
		Simple: func(t2 *testing.T, v any) { _, ok := v.(string); tt.Equal(t2, true, ok) },
		Eval:   cond,
		Equals: []*sliptest.EqTest{
			{Other: cond, Expect: true},
			{Other: slip.True, Expect: false},
		},
	}).Test(t)
	tt.Equal(t, "not a real control-error", cond.Error())
}

func TestControlErrorMake(t *testing.T) {
	tf := sliptest.Function{
		Source: `(make-condition 'Control-Error)`,
		Expect: "/^#<CONTROL-ERROR [0-9a-f]+>$/",
	}
	tf.Test(t)
	ce, ok := tf.Result.(slip.ControlError)
	tt.Equal(t, ok, true)
	tt.Equal(t, "/^#<CONTROL-ERROR [0-9a-f]+>$/", ce.Error())

	tf = sliptest.Function{
		Source: `(make-condition 'Control-Error :message "raise")`,
		Expect: "/^#<CONTROL-ERROR [0-9a-f]+>$/",
	}
	tf.Test(t)
	ce, ok = tf.Result.(slip.ControlError)
	tt.Equal(t, ok, true)
	tt.Equal(t, "raise", ce.Error())
}

func TestControlErrorPanic(t *testing.T) {
	tt.Panic(t, func() {
		slip.PanicControl("raise")
	})
}
