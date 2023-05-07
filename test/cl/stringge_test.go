// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestStringGeTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(string>= "abc" "abb")`,
		Expect: "2",
	}).Test(t)
}

func TestStringGeFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(string>= "abc" "abd")`,
		Expect: "nil",
	}).Test(t)
}

func TestStringGeShort(t *testing.T) {
	(&sliptest.Function{
		Source: `(string>= "abcd" "abc")`,
		Expect: "3",
	}).Test(t)
}

func TestStringGeEqual(t *testing.T) {
	(&sliptest.Function{
		Source: `(string>= "abc" "abc")`,
		Expect: "3",
	}).Test(t)
}
