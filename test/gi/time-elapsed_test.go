// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestTimeElapsedOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(time-elapsed @2024-11-24T12:00:00Z @2024-11-24T13:14:15Z)`,
		Expect: "4455",
	}).Test(t)
}

func TestTimeElapsedNotTime(t *testing.T) {
	(&sliptest.Function{
		Source:    `(time-elapsed t @2024-11-24T12:00:00Z)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(time-elapsed @2024-11-24T12:00:00Z t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
