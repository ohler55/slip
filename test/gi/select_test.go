// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/gi"
	"github.com/ohler55/slip/sliptest"
)

func TestSelectT0(t *testing.T) {
	(&sliptest.Function{
		Source: `(time-elapsed (now) (select ((time-after 0.01) x x)))`,
		Validate: func(t *testing.T, v slip.Object) {
			diff, ok := v.(slip.DoubleFloat)
			tt.Equal(t, true, ok)
			tt.Equal(t, true, 0.005 < diff && diff < 0.015)
		},
	}).Test(t)
}

func TestSelectT1(t *testing.T) {
	(&sliptest.Function{
		Source: `(select ((time-after 0.1) x 1) ((time-after 0.01) x (1+ 2)))`,
		Expect: "3",
	}).Test(t)
}

func TestSelectEmpty(t *testing.T) {
	(&sliptest.Function{
		Source: `(select ((time-after 0.1)))`,
		Expect: "nil",
	}).Test(t)
}

func TestSelectC0(t *testing.T) {
	scope := slip.NewScope()
	ch := make(gi.Channel, 1)
	ch <- slip.Fixnum(3)
	scope.Let("ch", ch)
	(&sliptest.Function{
		Scope: scope,
		Source: `(select (ch x (1+ x))
                         ((make-channel 1)))`,
		Expect: "4",
	}).Test(t)
}

func TestSelectC1(t *testing.T) {
	scope := slip.NewScope()
	ch := make(gi.Channel, 1)
	ch <- slip.Fixnum(3)
	scope.Let("ch", ch)
	(&sliptest.Function{
		Scope: scope,
		Source: `(select ((make-channel 1))
                         (ch x (1+ x))
                         ((make-channel 1)))`,
		Expect: "4",
	}).Test(t)
}

func TestSelectC2(t *testing.T) {
	scope := slip.NewScope()
	ch := make(gi.Channel, 1)
	ch <- slip.Fixnum(3)
	scope.Let("ch", ch)
	(&sliptest.Function{
		Scope: scope,
		Source: `(select ((make-channel 1))
                         ((make-channel 1))
                         (ch x (1+ x)))`,
		Expect: "4",
	}).Test(t)
}

func TestSelectC3(t *testing.T) {
	scope := slip.NewScope()
	ch := make(gi.Channel, 1)
	ch <- slip.Fixnum(3)
	scope.Let("ch", ch)
	(&sliptest.Function{
		Scope: scope,
		Source: `(select ((make-channel 1))
                         ((make-channel 1))
                         ((make-channel 1))
                         (ch x (1+ x)))`,
		Expect: "4",
	}).Test(t)
}

func TestSelectC4(t *testing.T) {
	scope := slip.NewScope()
	ch := make(gi.Channel, 1)
	ch <- slip.Fixnum(3)
	scope.Let("ch", ch)
	(&sliptest.Function{
		Scope: scope,
		Source: `(select ((make-channel 1))
                         ((make-channel 1))
                         ((make-channel 1))
                         ((make-channel 1))
                         (ch x (1+ x)))`,
		Expect: "4",
	}).Test(t)
}

func TestSelectC5(t *testing.T) {
	scope := slip.NewScope()
	ch := make(gi.Channel, 1)
	ch <- slip.Fixnum(3)
	scope.Let("ch", ch)
	(&sliptest.Function{
		Scope: scope,
		Source: `(select ((make-channel 1))
                         ((make-channel 1))
                         ((make-channel 1))
                         ((make-channel 1))
                         ((make-channel 1))
                         (ch x (1+ x)))`,
		Expect: "4",
	}).Test(t)
}

func TestSelectC6(t *testing.T) {
	scope := slip.NewScope()
	ch := make(gi.Channel, 1)
	ch <- slip.Fixnum(3)
	scope.Let("ch", ch)
	(&sliptest.Function{
		Scope: scope,
		Source: `(select ((make-channel 1))
                         ((make-channel 1))
                         ((make-channel 1))
                         ((make-channel 1))
                         ((make-channel 1))
                         ((make-channel 1))
                         (ch x (1+ x)))`,
		Expect: "4",
	}).Test(t)
}

func TestSelectC7(t *testing.T) {
	scope := slip.NewScope()
	ch := make(gi.Channel, 1)
	ch <- slip.Fixnum(3)
	scope.Let("ch", ch)
	(&sliptest.Function{
		Scope: scope,
		Source: `(select ((make-channel 1))
                         ((make-channel 1))
                         ((make-channel 1))
                         ((make-channel 1))
                         ((make-channel 1))
                         ((make-channel 1))
                         ((make-channel 1))
                         (ch x (1+ x)))`,
		Expect: "4",
	}).Test(t)
}

func TestSelectNotList(t *testing.T) {
	(&sliptest.Function{
		Source:    `(select t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestSelectNotChannel(t *testing.T) {
	(&sliptest.Function{
		Source:    `(select (t x x))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestSelectNotSymbol(t *testing.T) {
	(&sliptest.Function{
		Source:    `(select ((make-channel 1) t t))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestSelectManyTimeChannels(t *testing.T) {
	(&sliptest.Function{
		Source: `(select ((time-after 0.1) x 1) ((time-after 0.02) x 2) ((time-after 0.03) x 3))`,
		Expect: "2",
	}).Test(t)
}
