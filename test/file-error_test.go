// Copyright (c) 2023, Peter Ohler, All rights reserved.

package test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestFileErrorObj(t *testing.T) {
	cond := slip.NewFileError(slip.String("somefile"), "not a %s file-error", "real")
	(&sliptest.Object{
		Target: cond,
		String: "/^#<FILE-ERROR [0-9a-f]+>$/",
		Simple: func(t2 *testing.T, v any) { _, ok := v.(string); tt.Equal(t2, true, ok) },
		Eval:   cond,
		Equals: []*sliptest.EqTest{
			{Other: cond, Expect: true},
			{Other: slip.True, Expect: false},
		},
	}).Test(t)
	tt.Equal(t, "not a real file-error", cond.Error())
}

func TestFileErrorMake(t *testing.T) {
	tf := sliptest.Function{
		Source: `(make-condition 'File-Error)`,
		Expect: "/^#<file-error [0-9a-f]+>$/",
	}
	tf.Test(t)
	// TBD
	// ce, ok := tf.Result.(slip.FileError)
	// tt.Equal(t, ok, true)
	// tt.Equal(t, "/^#<FILE-ERROR [0-9a-f]+>$/", ce.Error())
	// tt.Equal(t, nil, ce.Pathname())

	tf = sliptest.Function{
		Source: `(make-condition 'File-Error :pathname "somefile" :message "raise")`,
		Expect: "/^#<file-error [0-9a-f]+>$/",
	}
	tf.Test(t)
	// TBD
	// ce, ok = tf.Result.(slip.FileError)
	// tt.Equal(t, ok, true)
	// tt.Equal(t, "raise", ce.Error())
	// tt.Equal(t, slip.String("somefile"), ce.Pathname())
}

func TestFileErrorPanic(t *testing.T) {
	tt.Panic(t, func() {
		slip.PanicFile(slip.String("somefile"), "raise")
	})
}
