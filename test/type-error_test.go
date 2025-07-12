// Copyright (c) 2023, Peter Ohler, All rights reserved.

package test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestTypeErrorObj(t *testing.T) {
	cond := slip.NewTypeErrorObject("testing", slip.Fixnum(3), "symbol")
	(&sliptest.Object{
		Target: cond,
		String: "/^#<type-error [0-9a-f]+>$/",
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
}

func TestTypeErrorMake(t *testing.T) {
	tf := sliptest.Function{
		Source: `(make-condition 'Type-Error :datum 3 :expected-type 'symbol)`,
		Expect: "/^#<type-error [0-9a-f]+>$/",
	}
	tf.Test(t)
	// TBD
	// te, ok := tf.Result.(slip.TypeError)
	// tt.Equal(t, ok, true)
	// tt.Equal(t, "testing must be a symbol not 3, a fixnum.", te.Error())
	// tt.Equal(t, slip.Fixnum(3), te.Datum())
	// tt.Equal(t, `"testing"`, slip.ObjectString(te.Context()))
	// tt.Equal(t, "(symbol)", slip.ObjectString(te.ExpectedTypes()))

	tf = sliptest.Function{
		Source: `(make-condition 'Type-Error :datum 3 :expected-type "symbol")`,
		Expect: "/^#<type-error [0-9a-f]+>$/",
	}
	tf.Test(t)
	// TBD
	// te, ok = tf.Result.(slip.TypeError)
	// tt.Equal(t, ok, true)
	// tt.Equal(t, "testing must be a symbol not 3, a fixnum.", te.Error())
	// tt.Equal(t, slip.Fixnum(3), te.Datum())
	// tt.Equal(t, `"testing"`, slip.ObjectString(te.Context()))
	// tt.Equal(t, "(symbol)", slip.ObjectString(te.ExpectedTypes()))

	tf = sliptest.Function{
		Source: `(make-condition 'Type-Error :datum 3 :expected-type '(symbol "string") )`,
		Expect: "/^#<type-error [0-9a-f]+>$/",
	}
	tf.Test(t)
	// TBD
	// te, ok = tf.Result.(slip.TypeError)
	// tt.Equal(t, ok, true)
	// tt.Equal(t, "testing must be a symbol or string not 3, a fixnum.", te.Error())
	// tt.Equal(t, slip.Fixnum(3), te.Datum())
	// tt.Equal(t, `"testing"`, slip.ObjectString(te.Context()))
	// tt.Equal(t, "(symbol string)", slip.ObjectString(te.ExpectedTypes()))

	tf = sliptest.Function{
		Source: `(make-condition 'Type-Error :message "a message")`,
		Expect: "/^#<type-error [0-9a-f]+>$/",
	}
	tf.Test(t)
	// TBD
	// te, ok = tf.Result.(slip.TypeError)
	// tt.Equal(t, ok, true)
	// tt.Equal(t, "a message", te.Error())
}

func TestTypeErrorPanic(t *testing.T) {
	tt.Panic(t, func() { slip.PanicType("testing", slip.Fixnum(3), "symbol") })
}
