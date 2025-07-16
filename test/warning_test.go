// Copyright (c) 2023, Peter Ohler, All rights reserved.

package test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/cl"
	"github.com/ohler55/slip/sliptest"
)

func TestWarningObj(t *testing.T) {
	warning := slip.NewWarning("a %s warning", "mock")
	(&sliptest.Object{
		Target: warning,
		String: "/^#<warning [0-9a-f]+>$/",
		Simple: func(t2 *testing.T, v any) {
			_, ok := v.(map[string]any)
			tt.Equal(t2, true, ok)
		},
		Eval: warning,
		Equals: []*sliptest.EqTest{
			{Other: warning, Expect: true},
			{Other: slip.True, Expect: false},
		},
	}).Test(t)
	tt.Equal(t, "a mock warning", cl.SimpleCondMsg(slip.NewScope(), warning.(slip.Instance)))
}

func TestWarningMake(t *testing.T) {
	tf := sliptest.Function{
		Source: `(make-condition 'warning :message "a warning")`,
		Expect: "/^#<warning [0-9a-f]+>$/",
	}
	tf.Test(t)
	cond, ok := tf.Result.(slip.Instance)
	tt.Equal(t, ok, true)
	value, has := cond.SlotValue(slip.Symbol("message"))
	tt.Equal(t, has, true)
	tt.Equal(t, slip.String("a warning"), value)
}
