// Copyright (c) 2023, Peter Ohler, All rights reserved.

package test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestParseErrorObj(t *testing.T) {
	cond := slip.NewParseError("not a %s parse-error", "real")
	(&sliptest.Object{
		Target: cond,
		String: "/^#<PARSE-ERROR [0-9a-f]+>$/",
		Simple: func(t2 *testing.T, v any) { _, ok := v.(string); tt.Equal(t2, true, ok) },
		Eval:   cond,
		Equals: []*sliptest.EqTest{
			{Other: cond, Expect: true},
			{Other: slip.True, Expect: false},
		},
	}).Test(t)
	tt.Equal(t, "not a real parse-error", cond.Error())
}

func TestParseErrorMake(t *testing.T) {
	tf := sliptest.Function{
		Source: `(make-condition 'Parse-Error)`,
		Expect: "/^#<parse-error [0-9a-f]+>$/",
	}
	tf.Test(t)
	// TBD
	// pe, ok := tf.Result.(slip.ParseError)
	// tt.Equal(t, ok, true)
	// tt.Equal(t, "/^#<PARSE-ERROR [0-9a-f]+>$/", pe.Error())

	tf = sliptest.Function{
		Source: `(make-condition 'Parse-Error :message "raise")`,
		Expect: "/^#<parse-error [0-9a-f]+>$/",
	}
	tf.Test(t)
	// TBD
	// pe, ok = tf.Result.(slip.ParseError)
	// tt.Equal(t, ok, true)
	// tt.Equal(t, "raise", pe.Error())
}

func TestParseErrorPanic(t *testing.T) {
	tt.Panic(t, func() { slip.PanicParse("raise") })
}
