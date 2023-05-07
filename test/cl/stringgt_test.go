// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestStringGtTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(string> "abc" "abb")`,
		Expect: "2",
	}).Test(t)
}

func TestStringGtFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(string> "abc" "abc")`,
		Expect: "nil",
	}).Test(t)
}

func TestStringGtShort(t *testing.T) {
	(&sliptest.Function{
		Source: `(string> "abcd" "abc")`,
		Expect: "3",
	}).Test(t)
}
