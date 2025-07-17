// Copyright (c) 2023, Peter Ohler, All rights reserved.

package test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/cl"
	"github.com/ohler55/slip/sliptest"
)

func TestStreamErrorObj(t *testing.T) {
	cond := slip.NewStreamError(&slip.OutputStream{}, "not a %s stream-error", "real")
	(&sliptest.Object{
		Target: cond,
		String: "/^#<stream-error [0-9a-f]+>$/",
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
	tt.Equal(t, "not a real stream-error", cl.SimpleCondMsg(slip.NewScope(), cond.(slip.Instance)))
}

func TestStreamErrorMake(t *testing.T) {
	tf := sliptest.Function{
		Source: `(make-condition 'Stream-Error :stream *standard-output* :message "raise")`,
		Expect: "/^#<stream-error [0-9a-f]+>$/",
	}
	tf.Test(t)
	cond, ok := tf.Result.(slip.Instance)
	tt.Equal(t, true, ok)
	value, has := cond.SlotValue(slip.Symbol("stream"))
	tt.Equal(t, true, has)
	tt.Equal(t, slip.StandardOutput, value)
	value, has = cond.SlotValue(slip.Symbol("message"))
	tt.Equal(t, true, has)
	tt.Equal(t, slip.String("raise"), value)
}

func TestStreamErrorPanic(t *testing.T) {
	tt.Panic(t, func() { slip.PanicStream(&slip.OutputStream{}, "raise") })
}
