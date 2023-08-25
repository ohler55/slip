// Copyright (c) 2023, Peter Ohler, All rights reserved.

package test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestProgramErrorObj(t *testing.T) {
	cond := slip.NewProgramError("not a %s program-error", "real")
	(&sliptest.Object{
		Target: cond,
		String: "/^#<PROGRAM-ERROR [0-9a-f]+>$/",
		Simple: func(t2 *testing.T, v any) { _, ok := v.(string); tt.Equal(t2, true, ok) },
		Eval:   cond,
		Equals: []*sliptest.EqTest{
			{Other: cond, Expect: true},
			{Other: slip.True, Expect: false},
		},
	}).Test(t)
	tt.Equal(t, "not a real program-error", cond.Error())
}

func TestProgramErrorMake(t *testing.T) {
	tf := sliptest.Function{
		Source: `(make-condition 'Program-Error)`,
		Expect: "/^#<PROGRAM-ERROR [0-9a-f]+>$/",
	}
	tf.Test(t)
	ce, ok := tf.Result.(slip.ProgramError)
	tt.Equal(t, ok, true)
	tt.Equal(t, "/^#<PROGRAM-ERROR [0-9a-f]+>$/", ce.Error())

	tf = sliptest.Function{
		Source: `(make-condition 'Program-Error :message "raise")`,
		Expect: "/^#<PROGRAM-ERROR [0-9a-f]+>$/",
	}
	tf.Test(t)
	ce, ok = tf.Result.(slip.ProgramError)
	tt.Equal(t, ok, true)
	tt.Equal(t, "raise", ce.Error())
}

func TestProgramErrorPanic(t *testing.T) {
	tt.Panic(t, func() {
		slip.PanicProgram("raise")
	})
}
