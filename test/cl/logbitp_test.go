// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestLogbitpFixnum(t *testing.T) {
	(&sliptest.Function{
		Source: `(logbitp 1 5)`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: `(logbitp 2 5)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(logbitp 2 -5)`,
		Expect: "nil",
	}).Test(t)
}

func TestLogbitpBignum(t *testing.T) {
	(&sliptest.Function{
		Source: `(logbitp 64 #x010203040506070809)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(logbitp 64 (- 0 #x010203040506070809))`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: `(logbitp 63 #x010203040506070809)`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: `(logbitp 63 (- 0 #x010203040506070809))`,
		Expect: "t",
	}).Test(t)
}

func TestLogbitpBadIndex(t *testing.T) {
	(&sliptest.Function{
		Source:    `(logbitp t 1)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestLogbitpBadInteger(t *testing.T) {
	(&sliptest.Function{
		Source:    `(logbitp 1 t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
