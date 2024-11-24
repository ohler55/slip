// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestTimeUnixOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(time-unix @2022-07-10T12:29:21-05:00)`,
		Expect: "1.657474161e+09",
	}).Test(t)
}

func TestTimeUnixBadTime(t *testing.T) {
	(&sliptest.Function{
		Source:    `(time-unix t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
