// Copyright (c) 2022, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/gi"
	"github.com/ohler55/slip/sliptest"
)

func TestChannel(t *testing.T) {
	ch := gi.Channel(make(chan slip.Object, 7))
	(&sliptest.Object{
		Target:    ch,
		String:    "#<channel 7>",
		Simple:    "#<channel 7>",
		Hierarchy: "channel.t",
		Equals: []*sliptest.EqTest{
			{Other: ch, Expect: true},
			{Other: slip.Fixnum(5), Expect: false},
		},
		Eval: ch,
	}).Test(t)
	tt.Equal(t, 0, ch.Length())
}
