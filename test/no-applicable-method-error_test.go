// Copyright (c) 2025, Peter Ohler, All rights reserved.

package test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/cl"
	"github.com/ohler55/slip/sliptest"
)

func TestNoApplicableMethodErrorObj(t *testing.T) {
	args := slip.List{slip.Fixnum(1), slip.Fixnum(2)}
	fi := slip.FindFunc("and")
	cond := slip.NewNoApplicableMethodError(fi, args, "")
	(&sliptest.Object{
		Target: cond,
		String: "/^#<no-applicable-method-error [0-9a-f]+>$/",
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
	tt.Equal(t, `There is no applicable method for the generic function
#<macro and>
  when called with arguments
    (1 2).`,
		cl.SimpleCondMsg(slip.NewScope(), cond.(slip.Instance)))
}

func TestNoApplicableMethodErrorMake(t *testing.T) {
	tf := sliptest.Function{
		Source: `(make-condition 'No-Applicable-Method-Error :function-arguments '(1 2))`,
		Expect: "/^#<no-applicable-method-error [0-9a-f]+>$/",
	}
	tf.Test(t)
	cond, ok := tf.Result.(slip.Instance)
	tt.Equal(t, true, ok)

	value, has := cond.SlotValue(slip.Symbol("generic-function"))
	tt.Equal(t, true, has)
	tt.Nil(t, value)

	value, has = cond.SlotValue(slip.Symbol("function-arguments"))
	tt.Equal(t, true, has)
	tt.Equal(t, slip.List{slip.Fixnum(1), slip.Fixnum(2)}, value)

	value, has = cond.SlotValue(slip.Symbol("message"))
	tt.Equal(t, true, has)
	tt.Nil(t, value)
}

func TestNoApplicableMethodErrorPanic(t *testing.T) {
	tt.Panic(t, func() { slip.PanicNoApplicableMethod(nil, slip.List{}, "raise") })
}
