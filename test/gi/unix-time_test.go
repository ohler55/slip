// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestUnixTimeSecond(t *testing.T) {
	(&sliptest.Function{
		Source: `(unix-time 1.657474161e+09)`,
		Expect: "@2022-07-10T17:29:21Z",
	}).Test(t)
	(&sliptest.Function{
		Source: `(unix-time 1.657474161e+09 :second)`,
		Expect: "@2022-07-10T17:29:21Z",
	}).Test(t)
}

func TestUnixTimeMillisecond(t *testing.T) {
	(&sliptest.Function{
		Source: `(unix-time 1.657474161e+12 :millisecond)`,
		Expect: "@2022-07-10T17:29:21Z",
	}).Test(t)
}

func TestUnixTimeMicrosecond(t *testing.T) {
	(&sliptest.Function{
		Source: `(unix-time 1.657474161e+15 :microsecond)`,
		Expect: "@2022-07-10T17:29:21Z",
	}).Test(t)
}

func TestUnixTimeNanosecond(t *testing.T) {
	(&sliptest.Function{
		Source: `(unix-time 1.657474161e+18 :nanosecond)`,
		Expect: "@2022-07-10T17:29:21Z",
	}).Test(t)
	(&sliptest.Function{
		Source: `(unix-time 1657474161000000000 :nanosecond)`,
		Expect: "@2022-07-10T17:29:21Z",
	}).Test(t)
}

func TestUnixTimeBadReal(t *testing.T) {
	(&sliptest.Function{
		Source:    `(unix-time t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestUnixTimeBadUnits(t *testing.T) {
	(&sliptest.Function{
		Source:    `(unix-time 12345 :hour)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
