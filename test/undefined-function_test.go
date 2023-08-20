// Copyright (c) 2023, Peter Ohler, All rights reserved.

package test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestUndefinedFunctionObj(t *testing.T) {
	cond := slip.NewUndefinedFunction(slip.Symbol("nothing"), "not a %s undefined-function", "real")
	(&sliptest.Object{
		Target: cond,
		String: "/^#<UNDEFINED-FUNCTION [0-9a-f]+>$/",
		Simple: func(t2 *testing.T, v any) { _, ok := v.(string); tt.Equal(t2, true, ok) },
		Eval:   cond,
		Equals: []*sliptest.EqTest{
			{Other: cond, Expect: true},
			{Other: slip.True, Expect: false},
		},
	}).Test(t)
	tt.Equal(t, "not a real undefined-function", cond.Error())
}

func TestUndefinedFunctionMake(t *testing.T) {
	tf := sliptest.Function{
		Source: `(make-condition 'Undefined-Function :name 'nothing)`,
		Expect: "/^#<UNDEFINED-FUNCTION [0-9a-f]+>$/",
	}
	tf.Test(t)
	us, ok := tf.Result.(slip.UndefinedFunction)
	tt.Equal(t, ok, true)
	tt.Equal(t, slip.Symbol("nothing"), us.Name())
	tt.Equal(t, "The function nothing is undefined.", us.Error())

	tf = sliptest.Function{
		Source: `(make-condition 'Undefined-Function :name 'nothing :message "raise")`,
		Expect: "/^#<UNDEFINED-FUNCTION [0-9a-f]+>$/",
	}
	tf.Test(t)
	us, ok = tf.Result.(slip.UndefinedFunction)
	tt.Equal(t, ok, true)
	tt.Equal(t, slip.Symbol("nothing"), us.Name())
	tt.Equal(t, "raise", us.Error())
}

func TestUndefinedFunctionPanic(t *testing.T) {
	tt.Panic(t, func() { slip.PanicUndefinedFunction(slip.Symbol("nothing"), "raise") })
}
