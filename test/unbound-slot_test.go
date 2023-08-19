// Copyright (c) 2023, Peter Ohler, All rights reserved.

package test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestUnboundSlotObj(t *testing.T) {
	cond := slip.NewUnboundSlot(slip.Symbol("vanilla"), slip.Symbol(":slop"), "not a %s unbound-slot", "real")
	(&sliptest.Object{
		Target: cond,
		String: "/^#<UNBOUND-SLOT [0-9a-f]+>$/",
		Simple: func(t2 *testing.T, v any) { _, ok := v.(string); tt.Equal(t2, true, ok) },
		Eval:   cond,
		Equals: []*sliptest.EqTest{
			{Other: cond, Expect: true},
			{Other: slip.True, Expect: false},
		},
	}).Test(t)
	tt.Equal(t, "not a real unbound-slot", cond.Error())
}

func TestUnboundSlotMake(t *testing.T) {
	tf := sliptest.Function{
		Source: `(make-condition 'Unbound-Slot :instance 'vanilla :name :slop)`,
		Expect: "/^#<UNBOUND-SLOT [0-9a-f]+>$/",
	}
	tf.Test(t)
	us, ok := tf.Result.(slip.UnboundSlot)
	tt.Equal(t, ok, true)
	tt.Equal(t, slip.Symbol("vanilla"), us.Instance())
	tt.Equal(t, slip.Symbol(":slop"), us.Name())
	tt.Equal(t, ":slop is not a slot in vanilla", us.Error())

	tf = sliptest.Function{
		Source: `(make-condition 'Unbound-Slot :instance 'vanilla :name :slop :message "raise")`,
		Expect: "/^#<UNBOUND-SLOT [0-9a-f]+>$/",
	}
	tf.Test(t)
	us, ok = tf.Result.(slip.UnboundSlot)
	tt.Equal(t, ok, true)
	tt.Equal(t, slip.Symbol("vanilla"), us.Instance())
	tt.Equal(t, slip.Symbol(":slop"), us.Name())
	tt.Equal(t, "raise", us.Error())
}

func TestUnboundSlotPanic(t *testing.T) {
	tt.Panic(t, func() { slip.PanicUnboundSlot(slip.Symbol("vanilla"), slip.Symbol(":slop"), "raise") })
}
