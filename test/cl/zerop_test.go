// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"math/big"
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestZerotpTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(zerop 0)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(zerop 0.0)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(zerop 0.0s0)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(zerop 0.0l0)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(zerop 0/4)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(zerop (coerce 0 'octet))`,
		Expect: "t",
	}).Test(t)
	scope := slip.NewScope()
	scope.Let(slip.Symbol("big"), (*slip.Bignum)(big.NewInt(0)))
	(&sliptest.Function{
		Scope:  scope,
		Source: `(zerop big)`,
		Expect: "t",
	}).Test(t)
}

func TestZerotpFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(zerop -1)`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: `(zerop 1)`,
		Expect: "nil",
	}).Test(t)
}

func TestZerotpBadArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(zerop)`,
		Panics: true,
	}).Test(t)
}

func TestZerotpNotNumber(t *testing.T) {
	(&sliptest.Function{
		Source: `(zerop t)`,
		Panics: true,
	}).Test(t)
}
