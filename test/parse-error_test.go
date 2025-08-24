// Copyright (c) 2023, Peter Ohler, All rights reserved.

package test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/cl"
	"github.com/ohler55/slip/sliptest"
)

func TestParseErrorObj(t *testing.T) {
	scope := slip.NewScope()
	cond := slip.ParseErrorNew(scope, 0, "not a %s parse-error", "real")
	(&sliptest.Object{
		Target: cond,
		String: "/^#<parse-error [0-9a-f]+>$/",
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
	tt.Equal(t, "not a real parse-error", cl.SimpleCondMsg(scope, cond.(slip.Instance)))
}

func TestParseErrorMake(t *testing.T) {
	tf := sliptest.Function{
		Source: `(make-condition 'Parse-Error :message "raise")`,
		Expect: "/^#<parse-error [0-9a-f]+>$/",
	}
	tf.Test(t)
	cond, ok := tf.Result.(slip.Instance)
	tt.Equal(t, true, ok)
	value, has := cond.SlotValue(slip.Symbol("message"))
	tt.Equal(t, true, has)
	tt.Equal(t, slip.String("raise"), value)
}

func TestParseErrorPanic(t *testing.T) {
	tt.Panic(t, func() { slip.ParsePanic(slip.NewScope(), 0, "raise") })
}
