// Copyright (c) 2025, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestUniversalToTimeOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(universal-to-time 3940406055)`,
		Expect: "@2024-11-12T13:14:15Z",
	}).Test(t)
}

func TestUniversalToTimeNotTime(t *testing.T) {
	(&sliptest.Function{
		Source:    `(universal-to-time t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
