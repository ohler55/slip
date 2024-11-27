// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"
	"time"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/gi"
	"github.com/ohler55/slip/sliptest"
)

func TestTimeChannel(t *testing.T) {
	tc := gi.TimeChannel(make(chan time.Time, 3))
	(&sliptest.Object{
		Target:    tc,
		String:    "#<time-channel 3>",
		Simple:    "#<time-channel 3>",
		Hierarchy: "time-channel.channel.t",
		Equals: []*sliptest.EqTest{
			{Other: tc, Expect: true},
			{Other: slip.Fixnum(5), Expect: false},
		},
		Eval: tc,
	}).Test(t)
	tt.Equal(t, 0, tc.Length())
}
