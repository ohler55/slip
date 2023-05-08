// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestStringNotLesspTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(string-not-lessp "abc" "abb")`,
		Expect: "2",
	}).Test(t)
}

func TestStringNotLesspEqual(t *testing.T) {
	(&sliptest.Function{
		Source: `(string-not-lessp "abc" "abc")`,
		Expect: "3",
	}).Test(t)
	(&sliptest.Function{
		Source: `(string-not-lessp "abc" "Abc")`,
		Expect: "3",
	}).Test(t)
}

func TestStringNotLesspFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(string-not-lessp "abc" "Abd")`,
		Expect: "nil",
	}).Test(t)
}

func TestStringNotLesspLong(t *testing.T) {
	(&sliptest.Function{
		Source: `(string-not-lessp "abc" "abcd")`,
		Expect: "nil",
	}).Test(t)
}

func TestStringNotLesspShort(t *testing.T) {
	(&sliptest.Function{
		Source: `(string-not-lessp "abcd" "abc")`,
		Expect: "3",
	}).Test(t)
}
