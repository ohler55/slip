// Copyright (c) 2023, Peter Ohler, All rights reserved.

package test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/cl"
	"github.com/ohler55/slip/sliptest"
)

func TestUnboundSlotObj(t *testing.T) {
	scope := slip.NewScope()
	cond := slip.UnboundSlotNew(scope, 0, slip.Symbol("vanilla"), slip.Symbol(":slop"), "not a %s unbound-slot", "real")
	(&sliptest.Object{
		Target: cond,
		String: "/^#<unbound-slot [0-9a-f]+>$/",
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
	tt.Equal(t, "not a real unbound-slot", cl.SimpleCondMsg(slip.NewScope(), cond.(slip.Instance)))
}

func TestUnboundSlotMake(t *testing.T) {
	tf := sliptest.Function{
		Source: `(make-condition 'Unbound-Slot :instance 'vanilla :name :slop :message "raise")`,
		Expect: "/^#<unbound-slot [0-9a-f]+>$/",
	}
	tf.Test(t)
	cond, ok := tf.Result.(slip.Instance)
	tt.Equal(t, true, ok)
	value, has := cond.SlotValue(slip.Symbol("name"))
	tt.Equal(t, true, has)
	tt.Equal(t, slip.Symbol(":slop"), value)
	value, has = cond.SlotValue(slip.Symbol("message"))
	tt.Equal(t, true, has)
	tt.Equal(t, slip.String("raise"), value)
}

func TestUnboundSlotNoFormat(t *testing.T) {
	scope := slip.NewScope()
	cond := slip.UnboundSlotNew(scope, 0, slip.Symbol("vanilla"), slip.Symbol(":slop"), "")
	tt.Equal(t, "The slot :slop is unbound in the object vanilla.", cl.SimpleCondMsg(scope, cond.(slip.Instance)))
}

func TestUnboundSlotPanic(t *testing.T) {
	tt.Panic(t, func() { slip.UnboundSlotPanic(nil, 0, slip.Symbol("vanilla"), slip.Symbol(":slop"), "raise") })
}
