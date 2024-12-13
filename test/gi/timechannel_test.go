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

func TestTimeChannelObj(t *testing.T) {
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

func TestTimeChannelRange(t *testing.T) {
	tc := make(chan time.Time, 3)
	tc <- time.Date(2024, time.December, 12, 18, 15, 11, 0, time.UTC)
	tc <- time.Date(2024, time.December, 12, 18, 15, 12, 0, time.UTC)
	close(tc)
	scope := slip.NewScope()
	scope.Let(slip.Symbol("tc"), gi.TimeChannel(tc))
	(&sliptest.Function{
		Scope:  scope,
		Source: `(let (result) (range (lambda (x) (addf result x)) tc) result)`,
		Expect: "(@2024-12-12T18:15:11Z @2024-12-12T18:15:12Z)",
	}).Test(t)
}
