// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/cl"
	"github.com/ohler55/slip/sliptest"
)

func TestSimpleTypeErrorObj(t *testing.T) {
	cond := cl.NewSimpleTypeError(nil, "condition ~A-~D", slip.Symbol("dummy"), slip.Fixnum(3))
	(&sliptest.Object{
		Target: cond,
		String: "/^#<SIMPLE-TYPE-ERROR [0-9a-f]+>$/",
		Simple: func(t2 *testing.T, v any) { _, ok := v.(string); tt.Equal(t2, true, ok) },
		Eval:   cond,
		Equals: []*sliptest.EqTest{
			{Other: cond, Expect: true},
			{Other: slip.True, Expect: false},
		},
	}).Test(t)
	tt.Equal(t, "condition dummy-3", cond.Error())
}

func TestSimpleTypeErrorMake(t *testing.T) {
	tf := sliptest.Function{
		Source: `(make-condition 'Simple-Type-Error)`,
		Expect: "/^#<SIMPLE-TYPE-ERROR [0-9a-f]+>$/",
	}
	tf.Test(t)
	sc, ok := tf.Result.(cl.SimpleTypeError)
	tt.Equal(t, ok, true)
	tt.Equal(t, "", sc.Error())

	tf = sliptest.Function{
		Source: `(make-condition 'Simple-Type-Error
                                 :format-control "condition ~A-~D"
                                 :format-arguments '(dummy 3)
                                 :datum 'day
                                 :context "con"
                                 :expected-type 'fixnum)`,
		Expect: "/^#<SIMPLE-TYPE-ERROR [0-9a-f]+>$/",
	}
	tf.Test(t)
	sc, ok = tf.Result.(cl.SimpleTypeError)
	tt.Equal(t, ok, true)
	tt.Equal(t, "condition dummy-3", sc.Error())
	tt.Equal(t, "condition ~A-~D", sc.Control())
	tt.Equal(t, "(dummy 3)", slip.ObjectString(sc.Arguments()))
	tt.Equal(t, slip.Symbol("day"), sc.Datum())
	tt.Equal(t, slip.String("con"), sc.Context())
	tt.Equal(t, "(fixnum)", slip.ObjectString(sc.ExpectedTypes()))
}

func TestSimpleTypeErrorMakeString(t *testing.T) {
	tf := sliptest.Function{
		Source: `(make-condition 'Simple-Type-Error :expected-type "fixnum")`,
		Expect: "/^#<SIMPLE-TYPE-ERROR [0-9a-f]+>$/",
	}
	tf.Test(t)
	ste, _ := tf.Result.(cl.SimpleTypeError)
	tt.Equal(t, "(fixnum)", slip.ObjectString(ste.ExpectedTypes()))
}

func TestSimpleTypeErrorMakeList(t *testing.T) {
	tf := sliptest.Function{
		Source: `(make-condition 'Simple-Type-Error :expected-type '("fixnum" float))`,
		Expect: "/^#<SIMPLE-TYPE-ERROR [0-9a-f]+>$/",
	}
	tf.Test(t)
	ste, _ := tf.Result.(cl.SimpleTypeError)
	tt.Equal(t, "(fixnum float)", slip.ObjectString(ste.ExpectedTypes()))
}

func TestSimpleTypeErrorMakeBadArgs(t *testing.T) {
	(&sliptest.Function{
		Source:    `(make-condition 'Simple-Type-Error :format-control t)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
	(&sliptest.Function{
		Source:    `(make-condition 'Simple-Type-Error :format-control "~A" :format-arguments t)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}

func TestPanicSimpleTypeError(t *testing.T) {
	tt.Panic(t, func() { cl.PanicSimpleTypeError(nil, "raise") })
}
