// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestTimeComponentsOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(time-components @2024-11-12T13:14:15.123456789Z)`,
		Expect: `(2024 11 12 13 14 15 123456789 "Tuesday")`,
	}).Test(t)
}

func TestTimeComponentsNotTime(t *testing.T) {
	(&sliptest.Function{
		Source:    `(time-components t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
