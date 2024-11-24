// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestTimeAddOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(time-add @2024-11-24T12:00:00Z 4455)`,
		Expect: "@2024-11-24T13:14:15Z",
	}).Test(t)
}

func TestTimeAddNotTime(t *testing.T) {
	(&sliptest.Function{
		Source:    `(time-add @2024-11-24T12:00:00Z t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(time-add t 1.23)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
