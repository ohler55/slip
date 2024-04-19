// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestLogcountFixnum(t *testing.T) {
	(&sliptest.Function{
		Source: `(logcount 5)`,
		Expect: "2",
	}).Test(t)
	(&sliptest.Function{
		Source: `(logcount 0)`,
		Expect: "0",
	}).Test(t)
	(&sliptest.Function{
		Source: `(logcount -5)`,
		Expect: "1",
	}).Test(t)
}

func TestLogcountBignum(t *testing.T) {
	(&sliptest.Function{
		Source: `(logcount #x010203040506070809)`,
		Expect: "15",
	}).Test(t)
	(&sliptest.Function{
		Source: `(logcount (- 0 #x010203040506070809))`,
		Expect: "14",
	}).Test(t)
}

func TestLogcountBadInteger(t *testing.T) {
	(&sliptest.Function{
		Source:    `(logcount t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
