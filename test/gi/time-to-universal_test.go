// Copyright (c) 2025, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestTimeToUniversalOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(time-to-universal @2024-11-12T13:14:15Z)`,
		Expect: "3940406055",
	}).Test(t)
}

func TestTimeToUniversalNotTime(t *testing.T) {
	(&sliptest.Function{
		Source:    `(time-to-universal t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
