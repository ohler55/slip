// Copyright (c) 2023, Peter Ohler, All rights reserved.

package test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestStreamErrorObj(t *testing.T) {
	cond := slip.NewStreamError(&slip.OutputStream{}, "not a %s stream-error", "real")
	(&sliptest.Object{
		Target: cond,
		String: "/^#<STREAM-ERROR [0-9a-f]+>$/",
		Simple: func(t2 *testing.T, v any) { _, ok := v.(string); tt.Equal(t2, true, ok) },
		Eval:   cond,
		Equals: []*sliptest.EqTest{
			{Other: cond, Expect: true},
			{Other: slip.True, Expect: false},
		},
	}).Test(t)
	tt.Equal(t, "not a real stream-error", cond.Error())
}

func TestStreamErrorMake(t *testing.T) {
	tf := sliptest.Function{
		Source: `(make-condition 'Stream-Error :stream (make-string-output-stream))`,
		Expect: "/^#<STREAM-ERROR [0-9a-f]+>$/",
	}
	tf.Test(t)
	se, ok := tf.Result.(slip.StreamError)
	tt.Equal(t, ok, true)
	tt.Equal(t, "#<OUTPUT-STREAM>", slip.ObjectString(se.Stream()))
	tt.Equal(t, "", se.Error())

	tf = sliptest.Function{
		Source: `(make-condition 'Stream-Error :stream (make-string-output-stream) :message "raise")`,
		Expect: "/^#<STREAM-ERROR [0-9a-f]+>$/",
	}
	tf.Test(t)
	se, ok = tf.Result.(slip.StreamError)
	tt.Equal(t, ok, true)
	tt.Equal(t, "#<OUTPUT-STREAM>", slip.ObjectString(se.Stream()))
	tt.Equal(t, "raise", se.Error())
}

func TestStreamErrorPanic(t *testing.T) {
	tt.Panic(t, func() { slip.PanicStream(&slip.OutputStream{}, "raise") })
}
