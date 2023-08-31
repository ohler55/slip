// Copyright (c) 2023, Peter Ohler, All rights reserved.

package test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestUnboundVariableObj(t *testing.T) {
	cond := slip.NewUnboundVariable(slip.Symbol("very"), "not a %s unbound-variable", "real")
	(&sliptest.Object{
		Target: cond,
		String: "/^#<UNBOUND-VARIABLE [0-9a-f]+>$/",
		Simple: func(t2 *testing.T, v any) { _, ok := v.(string); tt.Equal(t2, true, ok) },
		Eval:   cond,
		Equals: []*sliptest.EqTest{
			{Other: cond, Expect: true},
			{Other: slip.True, Expect: false},
		},
	}).Test(t)
	tt.Equal(t, "not a real unbound-variable", cond.Error())
}

func TestUnboundVariableMake(t *testing.T) {
	tf := sliptest.Function{
		Source: `(make-condition 'Unbound-Variable :name 'very)`,
		Expect: "/^#<UNBOUND-VARIABLE [0-9a-f]+>$/",
	}
	tf.Test(t)
	uv, ok := tf.Result.(slip.UnboundVariable)
	tt.Equal(t, ok, true)
	tt.Equal(t, slip.Symbol("very"), uv.Name())
	tt.Equal(t, "The variable very is unbound.", uv.Error())

	tf = sliptest.Function{
		Source: `(make-condition 'Unbound-Variable :name 'very :message "raise")`,
		Expect: "/^#<UNBOUND-VARIABLE [0-9a-f]+>$/",
	}
	tf.Test(t)
	uv, ok = tf.Result.(slip.UnboundVariable)
	tt.Equal(t, ok, true)
	tt.Equal(t, slip.Symbol("very"), uv.Name())
	tt.Equal(t, "raise", uv.Error())
}

func TestUnboundVariablePanic(t *testing.T) {
	tt.Panic(t, func() { slip.PanicUnboundVariable(slip.Symbol("very"), "raise") })
}
