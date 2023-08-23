// Copyright (c) 2023, Peter Ohler, All rights reserved.

package test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestReaderErrorObj(t *testing.T) {
	cond := slip.NewReaderError(slip.StandardOutput.(slip.Stream), "not a %s reader-error", "real")
	(&sliptest.Object{
		Target: cond,
		String: "/^#<READER-ERROR [0-9a-f]+>$/",
		Simple: func(t2 *testing.T, v any) { _, ok := v.(string); tt.Equal(t2, true, ok) },
		Eval:   cond,
		Equals: []*sliptest.EqTest{
			{Other: cond, Expect: true},
			{Other: slip.True, Expect: false},
		},
	}).Test(t)
	tt.Equal(t, "not a real reader-error", cond.Error())
}

func TestReaderErrorMake(t *testing.T) {
	tf := sliptest.Function{
		Source: `(make-condition 'Reader-Error)`,
		Expect: "/^#<READER-ERROR [0-9a-f]+>$/",
	}
	tf.Test(t)
	re, ok := tf.Result.(slip.ReaderError)
	tt.Equal(t, ok, true)
	tt.Equal(t, "", re.Error())
	tt.Nil(t, re.Stream())

	tf = sliptest.Function{
		Source: `(make-condition 'Reader-Error :stream *standard-output* :message "raise")`,
		Expect: "/^#<READER-ERROR [0-9a-f]+>$/",
	}
	tf.Test(t)
	re, ok = tf.Result.(slip.ReaderError)
	tt.Equal(t, ok, true)
	tt.Equal(t, "raise", re.Error())
	tt.Equal(t, slip.StandardOutput, re.Stream())
}

func TestReaderErrorPanic(t *testing.T) {
	tt.Panic(t, func() { slip.PanicReader(nil, "raise") })
}
