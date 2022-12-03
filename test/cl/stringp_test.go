// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestStringtpTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(stringp "abc")`,
		Expect: "t",
	}).Test(t)
}

func TestStringtpFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(stringp t)`,
		Expect: "nil",
	}).Test(t)
}

func TestStringtpBadArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(stringp)`,
		Panics: true,
	}).Test(t)
}
