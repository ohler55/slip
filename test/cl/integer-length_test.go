// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestIntegerLengthZero(t *testing.T) {
	(&sliptest.Function{
		Source: `(integer-length 0)`,
		Expect: "0",
	}).Test(t)
}

func TestIntegerLengthSmall(t *testing.T) {
	(&sliptest.Function{
		Source: `(integer-length 1)`,
		Expect: "1",
	}).Test(t)
	(&sliptest.Function{
		Source: `(integer-length 2)`,
		Expect: "2",
	}).Test(t)
	(&sliptest.Function{
		Source: `(integer-length 3)`,
		Expect: "2",
	}).Test(t)
	(&sliptest.Function{
		Source: `(integer-length 7)`,
		Expect: "3",
	}).Test(t)
}

func TestIntegerLengthLarge(t *testing.T) {
	(&sliptest.Function{
		Source: `(integer-length 65535)`,
		Expect: "16",
	}).Test(t)
	(&sliptest.Function{
		Source: `(integer-length 65536)`,
		Expect: "17",
	}).Test(t)
}

func TestIntegerLengthNeg(t *testing.T) {
	(&sliptest.Function{
		Source: `(integer-length -7)`,
		Expect: "3",
	}).Test(t)
	(&sliptest.Function{
		Source: `(integer-length -8)`,
		Expect: "3",
	}).Test(t)
}

func TestIntegerLengthBignum(t *testing.T) {
	(&sliptest.Function{
		Source: `(integer-length (- 10000000000000000000 10000000000000000000))`,
		Expect: "0",
	}).Test(t)
	(&sliptest.Function{
		Source: `(integer-length (- 10000000000000000007 10000000000000000000))`,
		Expect: "3",
	}).Test(t)
	(&sliptest.Function{
		Source: `(integer-length (- 10000000000000000000 10000000000000000007))`,
		Expect: "3",
	}).Test(t)
	(&sliptest.Function{
		Source: `(integer-length (- 10000000000000000000 10000000000000000008))`,
		Expect: "3",
	}).Test(t)
}

func TestIntegerLengthNotInteger(t *testing.T) {
	(&sliptest.Function{
		Source:    `(integer-length t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
