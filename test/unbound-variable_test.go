// Copyright (c) 2023, Peter Ohler, All rights reserved.

package test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/cl"
	"github.com/ohler55/slip/sliptest"
)

func TestUnboundVariableObj(t *testing.T) {
	scope := slip.NewScope()
	cond := slip.UnboundVariableNew(scope, 0, slip.Symbol("very"), "not a %s unbound-variable", "real")
	(&sliptest.Object{
		Target: cond,
		String: "/^#<unbound-variable [0-9a-f]+>$/",
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
	tt.Equal(t, "not a real unbound-variable", cl.SimpleCondMsg(scope, cond.(slip.Instance)))
}

func TestUnboundVariableMake(t *testing.T) {
	tf := sliptest.Function{
		Source: `(make-condition 'Unbound-Variable :name 'very :message "raise")`,
		Expect: "/^#<unbound-variable [0-9a-f]+>$/",
	}
	tf.Test(t)
	cond, ok := tf.Result.(slip.Instance)
	tt.Equal(t, true, ok)
	value, has := cond.SlotValue(slip.Symbol("name"))
	tt.Equal(t, true, has)
	tt.Equal(t, slip.Symbol("very"), value)
	value, has = cond.SlotValue(slip.Symbol("message"))
	tt.Equal(t, true, has)
	tt.Equal(t, slip.String("raise"), value)
}

func TestUnboundVariablePanic(t *testing.T) {
	tt.Panic(t, func() { slip.UnboundVariablePanic(nil, 0, slip.Symbol("very"), "raise") })
}
