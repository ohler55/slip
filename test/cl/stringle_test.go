// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestStringLeTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(string<= "abc" "abd")`,
		Expect: "2",
	}).Test(t)
}

func TestStringLeFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(string<= "abc" "abb")`,
		Expect: "nil",
	}).Test(t)
}

func TestStringLeShort(t *testing.T) {
	(&sliptest.Function{
		Source: `(string<= "abc" "abcd")`,
		Expect: "3",
	}).Test(t)
}

func TestStringLeEqual(t *testing.T) {
	(&sliptest.Function{
		Source: `(string<= "abc" "abc")`,
		Expect: "3",
	}).Test(t)
}
