// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestTimeTickerOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(let* ((tc (time-ticker 0.01)))
                  (time-elapsed (channel-pop tc) (channel-pop tc)))`,
		Validate: func(t *testing.T, v slip.Object) {
			diff, ok := v.(slip.DoubleFloat)
			tt.Equal(t, true, ok)
			tt.Equal(t, true, 0.005 < diff && diff < 0.015)
		},
	}).Test(t)
}

func TestTimeTickerBadDuration(t *testing.T) {
	(&sliptest.Function{
		Source:    `(time-ticker t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
