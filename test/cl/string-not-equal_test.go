// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestStringNotEqualTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(string-not-equal "abc" "abd")`,
		Expect: "2",
	}).Test(t)
}

func TestStringNotEqualEqual(t *testing.T) {
	(&sliptest.Function{
		Source: `(string-not-equal "abc" "abc")`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: `(string-not-equal "abc" "Abc")`,
		Expect: "nil",
	}).Test(t)
}

func TestStringNotEqualFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(string-not-equal "abc" "Abb")`,
		Expect: "2",
	}).Test(t)
}

func TestStringNotEqualLong(t *testing.T) {
	(&sliptest.Function{
		Source: `(string-not-equal "abc" "abcd")`,
		Expect: "3",
	}).Test(t)
}

func TestStringNotEqualShort(t *testing.T) {
	(&sliptest.Function{
		Source: `(string-not-equal "abcd" "abc")`,
		Expect: "3",
	}).Test(t)
}
