// Copyright (c) 2023, Peter Ohler, All rights reserved.

package test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
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
		String: "/^#<METHOD-ERROR [0-9a-f]+>$/",
		Simple: func(t2 *testing.T, v any) { _, ok := v.(string); tt.Equal(t2, true, ok) },
		Eval:   cond,
		Equals: []*sliptest.EqTest{
			{Other: cond, Expect: true},
			{Other: slip.True, Expect: false},
		},
	}).Test(t)
	tt.Equal(t, "not a real method-error", cond.Error())
}

func TestMethodErrorMake(t *testing.T) {
	tf := sliptest.Function{
		Source: `(make-condition 'Method-Error :class 'vanilla :name :meth :qualifier :nonsense)`,
		Expect: "/^#<METHOD-ERROR [0-9a-f]+>$/",
	}
	tf.Test(t)
	us, ok := tf.Result.(slip.MethodError)
	tt.Equal(t, ok, true)
	tt.Equal(t, slip.Symbol("vanilla"), us.Class())
	tt.Equal(t, slip.Symbol(":nonsense"), us.Qualifier())
	tt.Equal(t, slip.Symbol(":meth"), us.Name())
	tt.Equal(t, ":nonsense :meth is not a valid method combination for vanilla.", us.Error())

	tf = sliptest.Function{
		Source: `(make-condition 'Method-Error :class 'vanilla :name :meth :message "raise")`,
		Expect: "/^#<METHOD-ERROR [0-9a-f]+>$/",
	}
	tf.Test(t)
	us, ok = tf.Result.(slip.MethodError)
	tt.Equal(t, ok, true)
	tt.Equal(t, slip.Symbol("vanilla"), us.Class())
	tt.Equal(t, nil, us.Qualifier())
	tt.Equal(t, slip.Symbol(":meth"), us.Name())
	tt.Equal(t, "raise", us.Error())
}

func TestMethodErrorPanic(t *testing.T) {
	tt.Panic(t, func() { slip.PanicMethod(slip.Symbol("vanilla"), slip.Symbol(":meth"), nil, "raise") })
}
