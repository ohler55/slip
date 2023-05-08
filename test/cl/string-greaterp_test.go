// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestStringGreaterpTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(string-greaterp "abc" "Abb")`,
		Expect: "2",
	}).Test(t)
}

func TestStringGreaterpEqual(t *testing.T) {
	(&sliptest.Function{
		Source: `(string-greaterp "abc" "abc")`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: `(string-greaterp "abc" "Abc")`,
		Expect: "nil",
	}).Test(t)
}

func TestStringGreaterpFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(string-greaterp "abc" "Abd")`,
		Expect: "nil",
	}).Test(t)
}

func TestStringGreaterpLong(t *testing.T) {
	(&sliptest.Function{
		Source: `(string-greaterp "abcd" "abc")`,
		Expect: "3",
	}).Test(t)
}

func TestStringGreaterpShort(t *testing.T) {
	(&sliptest.Function{
		Source: `(string-greaterp "abc" "abcd")`,
		Expect: "nil",
	}).Test(t)
}
