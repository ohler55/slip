// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestStringNotGreaterpTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(string-not-greaterp "abc" "abd")`,
		Expect: "2",
	}).Test(t)
}

func TestStringNotGreaterpEqual(t *testing.T) {
	(&sliptest.Function{
		Source: `(string-not-greaterp "abc" "abc")`,
		Expect: "3",
	}).Test(t)
	(&sliptest.Function{
		Source: `(string-not-greaterp "abc" "Abc")`,
		Expect: "3",
	}).Test(t)
}

func TestStringNotGreaterpFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(string-not-greaterp "abc" "Abb")`,
		Expect: "nil",
	}).Test(t)
}

func TestStringNotGreaterpLong(t *testing.T) {
	(&sliptest.Function{
		Source: `(string-not-greaterp "abc" "abcd")`,
		Expect: "3",
	}).Test(t)
}

func TestStringNotGreaterpShort(t *testing.T) {
	(&sliptest.Function{
		Source: `(string-not-greaterp "abcd" "abc")`,
		Expect: "nil",
	}).Test(t)
}
