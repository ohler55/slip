// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestNumbertpTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(numberp 4)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(numberp 4.1)`,
		Expect: "t",
	}).Test(t)
}

func TestNumbertpFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(numberp t)`,
		Expect: "nil",
	}).Test(t)
}

func TestNumbertpBadArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(numberp)`,
		Panics: true,
	}).Test(t)
}
