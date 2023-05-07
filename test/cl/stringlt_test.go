// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestStringLtTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(string< "abc" "abd")`,
		Expect: "2",
	}).Test(t)
}

func TestStringLtFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(string< "abc" "abc")`,
		Expect: "nil",
	}).Test(t)
}

func TestStringLtShort(t *testing.T) {
	(&sliptest.Function{
		Source: `(string< "abc" "abcd")`,
		Expect: "3",
	}).Test(t)
}
