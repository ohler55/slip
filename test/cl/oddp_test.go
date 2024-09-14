// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestOddtpTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(oddp 5)`,
		Expect: "t",
	}).Test(t)
}

func TestOddtpFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(oddp 4)`,
		Expect: "nil",
	}).Test(t)
}

func TestOddtpBigTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(oddp 12297829382473034411)`,
		Expect: "t",
	}).Test(t)
}

func TestOddtpBigFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(oddp 12297829382473034410)`,
		Expect: "nil",
	}).Test(t)
}

func TestOddtpOctet(t *testing.T) {
	(&sliptest.Function{
		Source: `(oddp (coerce 5 'octet))`,
		Expect: "t",
	}).Test(t)
}

func TestOddtpBadArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(oddp)`,
		Panics: true,
	}).Test(t)
}

func TestOddtpNotInteger(t *testing.T) {
	(&sliptest.Function{
		Source: `(oddp 7.0)`,
		Panics: true,
	}).Test(t)
}
