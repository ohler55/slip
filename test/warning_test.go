// Copyright (c) 2023, Peter Ohler, All rights reserved.

package test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestWarningObj(t *testing.T) {
	warning := slip.NewWarning("a %s warning", "mock")
	(&sliptest.Object{
		Target: warning,
		String: "/^#<WARNING [0-9a-f]+>$/",
		Simple: func(t2 *testing.T, v any) { _, ok := v.(string); tt.Equal(t2, true, ok) },
		Eval:   warning,
		Equals: []*sliptest.EqTest{
			{Other: warning, Expect: true},
			{Other: slip.True, Expect: false},
		},
	}).Test(t)
	tt.Equal(t, "a mock warning", warning.Message())
}

func TestWarningMake(t *testing.T) {
	tf := sliptest.Function{
		Source: `(make-condition 'warning :message "a warning")`,
		Expect: "/^#<WARNING [0-9a-f]+>$/",
	}
	tf.Test(t)
	warning, ok := tf.Result.(slip.Warning)
	tt.Equal(t, ok, true)
	tt.Equal(t, "a warning", warning.Message())
	tt.Equal(t, "a warning", warning.Error())
}
