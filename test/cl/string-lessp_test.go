// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestStringLesspTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(string-lessp "abc" "Abd")`,
		Expect: "2",
	}).Test(t)
}

func TestStringLesspEqual(t *testing.T) {
	(&sliptest.Function{
		Source: `(string-lessp "abc" "abc")`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: `(string-lessp "abc" "Abc")`,
		Expect: "nil",
	}).Test(t)
}

func TestStringLesspFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(string-lessp "abc" "Abb")`,
		Expect: "nil",
	}).Test(t)
}

func TestStringLesspLong(t *testing.T) {
	(&sliptest.Function{
		Source: `(string-lessp "abc" "abcd")`,
		Expect: "3",
	}).Test(t)
}

func TestStringLesspShort(t *testing.T) {
	(&sliptest.Function{
		Source: `(string-lessp "abcd" "abc")`,
		Expect: "nil",
	}).Test(t)
}
