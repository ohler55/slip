// Copyright (c) 2023, Peter Ohler, All rights reserved.

package test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/cl"
	"github.com/ohler55/slip/sliptest"
)

func TestFileErrorObj(t *testing.T) {
	cond := slip.NewFileError(slip.String("somefile"), "not a %s file-error", "real")
	(&sliptest.Object{
		Target: cond,
		String: "/^#<file-error [0-9a-f]+>$/",
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
	tt.Equal(t, "not a real file-error", cl.SimpleCondMsg(slip.NewScope(), cond.(slip.Instance)))
}

func TestFileErrorMake(t *testing.T) {
	tf := sliptest.Function{
		Source: `(make-condition 'File-Error)`,
		Expect: "/^#<file-error [0-9a-f]+>$/",
	}
	tf.Test(t)
	cond, ok := tf.Result.(slip.Instance)
	tt.Equal(t, ok, true)
	value, has := cond.SlotValue(slip.Symbol("pathname"))
	tt.Equal(t, has, true)
	tt.Equal(t, nil, value)

	tf = sliptest.Function{
		Source: `(make-condition 'File-Error :pathname "somefile" :message "raise")`,
		Expect: "/^#<file-error [0-9a-f]+>$/",
	}
	tf.Test(t)
	cond, ok = tf.Result.(slip.Instance)
	tt.Equal(t, ok, true)
	value, has = cond.SlotValue(slip.Symbol("pathname"))
	tt.Equal(t, has, true)
	tt.Equal(t, slip.String("somefile"), value)
}

func TestFileErrorPanic(t *testing.T) {
	tt.Panic(t, func() {
		slip.PanicFile(slip.String("somefile"), "raise")
	})
}
