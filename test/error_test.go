// Copyright (c) 2023, Peter Ohler, All rights reserved.

package test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestErrorObj(t *testing.T) {
	err := slip.NewError("not a %s error", "real")
	(&sliptest.Object{
		Target: err,
		String: "/^#<ERROR [0-9a-f]+>$/",
		Simple: func(t2 *testing.T, v any) { _, ok := v.(string); tt.Equal(t2, true, ok) },
		Eval:   err,
		Equals: []*sliptest.EqTest{
			{Other: err, Expect: true},
			{Other: slip.True, Expect: false},
		},
	}).Test(t)
	tt.Equal(t, "not a real error", err.Error())
}

func TestErrorMake(t *testing.T) {
	tf := sliptest.Function{
		Source: `(make-condition 'Error)`,
		Expect: "/^#<error [0-9a-f]+>$/",
	}
	tf.Test(t)
	// TBD
	// e, ok := tf.Result.(slip.Error)
	// tt.Equal(t, ok, true)
	// tt.Equal(t, "/^#<error [0-9a-f]+>$/", e.Error())

	tf = sliptest.Function{
		Source: `(make-condition 'Error :message "raise")`,
		Expect: "/^#<error [0-9a-f]+>$/",
	}
	tf.Test(t)
	// TBD
	// e, ok = tf.Result.(slip.Error)
	// tt.Equal(t, ok, true)
	// tt.Equal(t, "raise", e.Error())
	// // Created outside a function so stack should be empty.
	// tt.Equal(t, 0, len(e.Stack()))
}
