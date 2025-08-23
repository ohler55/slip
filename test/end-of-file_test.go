// Copyright (c) 2025, Peter Ohler, All rights reserved.

package test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/cl"
	"github.com/ohler55/slip/sliptest"
)

func TestEndOfFileObj(t *testing.T) {
	scope := slip.NewScope()
	cond := slip.EndOfFileNew(scope, 0, &slip.OutputStream{}, "not a %s end-of-file", "real")
	(&sliptest.Object{
		Target: cond,
		String: "/^#<end-of-file [0-9a-f]+>$/",
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
	tt.Equal(t, "not a real end-of-file", cl.SimpleCondMsg(scope, cond.(slip.Instance)))
}

func TestEndOfFileMake(t *testing.T) {
	tf := sliptest.Function{
		Source: `(make-condition 'End-Of-File :stream (make-string-output-stream))`,
		Expect: "/^#<end-of-file [0-9a-f]+>$/",
	}
	tf.Test(t)
	cond, ok := tf.Result.(slip.Instance)
	tt.Equal(t, ok, true)
	value, has := cond.SlotValue(slip.Symbol("stream"))
	tt.Equal(t, has, true)
	tt.Equal(t, "#<STRING-STREAM>", value.String())

	value, has = cond.SlotValue(slip.Symbol("message"))
	tt.Equal(t, has, true)
	tt.Equal(t, slip.Unbound, value)

	tf = sliptest.Function{
		Source: `(make-condition 'end-of-file :stream (make-string-output-stream) :message "raise")`,
		Expect: "/^#<end-of-file [0-9a-f]+>$/",
	}
	tf.Test(t)
	cond, ok = tf.Result.(slip.Instance)
	tt.Equal(t, ok, true)
	value, has = cond.SlotValue(slip.Symbol("stream"))
	tt.Equal(t, has, true)
	tt.Equal(t, "#<STRING-STREAM>", value.String())

	value, has = cond.SlotValue(slip.Symbol("message"))
	tt.Equal(t, has, true)
	tt.Equal(t, slip.String("raise"), value)
}

func TestEndOfFilePanic(t *testing.T) {
	tt.Panic(t, func() { slip.EndOfFilePanic(nil, 0, &slip.OutputStream{}, "raise") })
}
