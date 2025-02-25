// Copyright (c) 2025, Peter Ohler, All rights reserved.

package test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestEndOfFileObj(t *testing.T) {
	cond := slip.NewEndOfFile(&slip.OutputStream{}, "not a %s end-of-file", "real")
	(&sliptest.Object{
		Target: cond,
		String: "/^#<END-OF-FILE [0-9a-f]+>$/",
		Simple: func(t2 *testing.T, v any) { _, ok := v.(string); tt.Equal(t2, true, ok) },
		Eval:   cond,
		Equals: []*sliptest.EqTest{
			{Other: cond, Expect: true},
			{Other: slip.True, Expect: false},
		},
	}).Test(t)
	tt.Equal(t, "not a real end-of-file", cond.Error())
}

func TestEndOfFileMake(t *testing.T) {
	tf := sliptest.Function{
		Source: `(make-condition 'End-Of-File :stream (make-string-output-stream))`,
		Expect: "/^#<END-OF-FILE [0-9a-f]+>$/",
	}
	tf.Test(t)
	se, ok := tf.Result.(*slip.EndOfFilePanic)
	tt.Equal(t, ok, true)
	tt.Equal(t, "#<STRING-STREAM>", slip.ObjectString(se.Stream()))
	tt.Equal(t, "/^#<END-OF-FILE [0-9a-f]+>$/", se.Error())

	tf = sliptest.Function{
		Source: `(make-condition 'end-of-file :stream (make-string-output-stream) :message "raise")`,
		Expect: "/^#<END-OF-FILE [0-9a-f]+>$/",
	}
	tf.Test(t)
	se, ok = tf.Result.(*slip.EndOfFilePanic)
	tt.Equal(t, ok, true)
	tt.Equal(t, "#<STRING-STREAM>", slip.ObjectString(se.Stream()))
	tt.Equal(t, "raise", se.Error())
}

func TestEndOfFilePanic(t *testing.T) {
	tt.Panic(t, func() { slip.PanicEndOfFile(&slip.OutputStream{}, "raise") })
}
