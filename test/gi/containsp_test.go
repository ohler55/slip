// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestContainspTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(containsp "abcdef" "cd")`,
		Expect: "t",
	}).Test(t)
}

func TestContainspFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(containsp "abcdef" "ac")`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: `(containsp "abcdef" "CD")`,
		Expect: "nil",
	}).Test(t)
}

func TestContainspIgnoreCase(t *testing.T) {
	(&sliptest.Function{
		Source: `(containsp "abcdef" "CD" :ignore-case t)`,
		Expect: "t",
	}).Test(t)
}

func TestContainspBadString(t *testing.T) {
	(&sliptest.Function{
		Source:    `(containsp t "a")`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}

func TestContainspBadSub(t *testing.T) {
	(&sliptest.Function{
		Source:    `(containsp "abc" t)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}
