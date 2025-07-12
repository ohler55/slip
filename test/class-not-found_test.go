// Copyright (c) 2023, Peter Ohler, All rights reserved.

package test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestClassNotFoundObj(t *testing.T) {
	cond := slip.NewClassNotFound(slip.Symbol("klas"), "not a %s class-not-found", "real")
	(&sliptest.Object{
		Target: cond,
		String: "/^#<CLASS-NOT-FOUND [0-9a-f]+>$/",
		Simple: func(t2 *testing.T, v any) { _, ok := v.(string); tt.Equal(t2, true, ok) },
		Eval:   cond,
		Equals: []*sliptest.EqTest{
			{Other: cond, Expect: true},
			{Other: slip.True, Expect: false},
		},
	}).Test(t)
	tt.Equal(t, "not a real class-not-found", cond.Error())
}

func TestClassNotFoundMake(t *testing.T) {
	tf := sliptest.Function{
		Source: `(make-condition 'Class-Not-Found :name 'klas)`,
		Expect: "/^#<class-not-found [0-9a-f]+>$/",
	}
	tf.Test(t)
	// TBD
	// ce, ok := tf.Result.(slip.ClassNotFound)
	// tt.Equal(t, ok, true)
	// tt.Equal(t, slip.Symbol("klas"), ce.Name())
	// tt.Equal(t, "class klas not found", ce.Error())

	tf = sliptest.Function{
		Source: `(make-condition 'class-not-found :name 'klas)`,
		Expect: "/^#<class-not-found [0-9a-f]+>$/",
	}
	tf.Test(t)
	// TBD
	// ce, ok = tf.Result.(slip.ClassNotFound)
	// tt.Equal(t, ok, true)
	// tt.Equal(t, slip.Symbol("klas"), ce.Name())
	// tt.Equal(t, "class klas not found", ce.Error())
}

func TestClassNotFoundPanic(t *testing.T) {
	tt.Panic(t, func() { slip.PanicClassNotFound(slip.Symbol("klas"), "raise") })
}
