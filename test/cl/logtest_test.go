// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestLogtestFixnum(t *testing.T) {
	(&sliptest.Function{
		Source: `(logtest 5 4)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(logtest 5 2)`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: `(logtest -5 -2)`,
		Expect: "t",
	}).Test(t)
}

func TestLogtestBignum(t *testing.T) {
	(&sliptest.Function{
		Source: `(logtest #x33333333333333333333 #x222222222222222222)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(logtest #x44444444444444444444 #x222222222222222222)`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: `(logtest #x333333333333333333 #x22222222222222222222)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(logtest #x444444444444444444 #x22222222222222222222)`,
		Expect: "nil",
	}).Test(t)
}

func TestLogtestMixed(t *testing.T) {
	(&sliptest.Function{
		Source: `(logtest #x333333333333333333 #x2222222222222222)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(logtest #x444444444444444444 #x2222222222222222)`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: `(logtest #x33333333333333 #x22222222222222222222)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(logtest #x44444444444444 #x22222222222222222222)`,
		Expect: "nil",
	}).Test(t)
}

func TestLogtestNotInteger(t *testing.T) {
	(&sliptest.Function{
		Source:    `(logtest t 1)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(logtest 1 t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(logtest #x1234567890abcdef0 t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
