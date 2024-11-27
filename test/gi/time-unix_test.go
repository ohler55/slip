// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestTimeUnixSecond(t *testing.T) {
	(&sliptest.Function{
		Source: `(time-unix @2022-07-10T12:29:21-05:00)`,
		Expect: "1.657474161e+09",
	}).Test(t)
	(&sliptest.Function{
		Source: `(time-unix @2022-07-10T12:29:21-05:00 :second)`,
		Expect: "1.657474161e+09",
	}).Test(t)
}

func TestTimeUnixMillisecond(t *testing.T) {
	(&sliptest.Function{
		Source: `(time-unix @2022-07-10T12:29:21-05:00 :millisecond)`,
		Expect: "1.657474161e+12",
	}).Test(t)
}

func TestTimeUnixMicrosecond(t *testing.T) {
	(&sliptest.Function{
		Source: `(time-unix @2022-07-10T12:29:21-05:00 :microsecond)`,
		Expect: "1.657474161e+15",
	}).Test(t)
}

func TestTimeUnixNanosecond(t *testing.T) {
	(&sliptest.Function{
		Source: `(time-unix @2022-07-10T12:29:21-05:00 :nanosecond)`,
		Expect: "1657474161000000000",
	}).Test(t)
}

func TestTimeUnixBadTime(t *testing.T) {
	(&sliptest.Function{
		Source:    `(time-unix t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestTimeUnixBadUnits(t *testing.T) {
	(&sliptest.Function{
		Source:    `(time-unix @2022-07-10T12:29:21-05:00 :hour)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
