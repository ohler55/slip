// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestTimeAfterOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(let* ((tc (time-after 0.01))
                        (t0 (now)))
                  (- (time-unix (channel-pop tc)) (time-unix t0)))`,
		Validate: func(t *testing.T, v slip.Object) {
			diff, ok := v.(slip.DoubleFloat)
			tt.Equal(t, true, ok)
			tt.Equal(t, true, 0.005 < diff && diff < 0.015)
		},
	}).Test(t)
}

func TestTimeAfterBadDuration(t *testing.T) {
	(&sliptest.Function{
		Source:    `(time-after t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
