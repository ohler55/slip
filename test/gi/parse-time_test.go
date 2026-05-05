// Copyright (c) 2026, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestParseTimeDefaults(t *testing.T) {
	(&sliptest.Function{
		Source: `(parse-time "2026-05-05T05:05:05.555Z")`,
		Expect: "@2026-05-05T05:05:05.555Z",
	}).Test(t)
}

func TestParseTimeLayout(t *testing.T) {
	(&sliptest.Function{
		Source: `(parse-time "2026-05-05 05:05:05.555" "2006-01-02 15:04:05.999")`,
		Expect: "@2026-05-05T05:05:05.555Z",
	}).Test(t)
}

func TestParseTimeLocation(t *testing.T) {
	(&sliptest.Function{
		Source: `(parse-time "2026-05-05 05:05:05.555" "2006-01-02 15:04:05.999" "EST")`,
		Expect: "@2026-05-05T05:05:05.555-05:00",
	}).Test(t)
}

func TestParseTimeNotString(t *testing.T) {
	(&sliptest.Function{
		Source:    `(parse-time t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestParseTimeLayoutNotString(t *testing.T) {
	(&sliptest.Function{
		Source:    `(parse-time "2026-05-05" t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestParseTimeLocationNotString(t *testing.T) {
	(&sliptest.Function{
		Source:    `(parse-time "2026-05-05" "2006-01-02" t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestParseTimeBadLocation(t *testing.T) {
	(&sliptest.Function{
		Source:    `(parse-time "2026-05-05" "2006-01-02" "quux")`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestParseTimeFail(t *testing.T) {
	(&sliptest.Function{
		Source:    `(parse-time "2026-05-05" "2006-01-02T15:04:05")`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}
