// Copyright (c) 2023, Peter Ohler, All rights reserved.

package test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/cl"
	"github.com/ohler55/slip/sliptest"
)

func TestClassNotFoundObj(t *testing.T) {
	cond := slip.NewClassNotFound(slip.Symbol("klas"), "not a %s class-not-found", "real")
	(&sliptest.Object{
		Target: cond,
		String: "/^#<class-not-found [0-9a-f]+>$/",
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
	tt.Equal(t, "not a real class-not-found", cl.SimpleCondMsg(slip.NewScope(), cond.(slip.Instance)))
}

func TestClassNotFoundMake(t *testing.T) {
	tf := sliptest.Function{
		Source: `(make-condition 'Class-Not-Found :name 'klas)`,
		Expect: "/^#<class-not-found [0-9a-f]+>$/",
	}
	tf.Test(t)
	cond, ok := tf.Result.(slip.Instance)
	tt.Equal(t, true, ok)
	value, has := cond.SlotValue(slip.Symbol("name"))
	tt.Equal(t, true, has)
	tt.Equal(t, slip.Symbol("klas"), value)
	value, has = cond.SlotValue(slip.Symbol("message"))
	tt.Equal(t, true, has)
	tt.Nil(t, value)

	tf = sliptest.Function{
		Source: `(make-condition 'class-not-found :name 'klas :message "raise")`,
		Expect: "/^#<class-not-found [0-9a-f]+>$/",
	}
	tf.Test(t)
	cond, ok = tf.Result.(slip.Instance)
	tt.Equal(t, true, ok)
	value, has = cond.SlotValue(slip.Symbol("name"))
	tt.Equal(t, true, has)
	tt.Equal(t, slip.Symbol("klas"), value)
	value, has = cond.SlotValue(slip.Symbol("message"))
	tt.Equal(t, true, has)
	tt.Equal(t, slip.String("raise"), value)
}

func TestClassNotFoundPanic(t *testing.T) {
	tt.Panic(t, func() { slip.PanicClassNotFound(slip.Symbol("klas"), "raise") })
}
