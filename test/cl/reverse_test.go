// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestReverseNil(t *testing.T) {
	(&sliptest.Function{
		Source: `(reverse nil)`,
		Expect: "nil",
	}).Test(t)
}

func TestReverseList(t *testing.T) {
	(&sliptest.Function{
		Source: `(reverse '(a b c d))`,
		Expect: "(d c b a)",
	}).Test(t)
	(&sliptest.Function{
		Source: `(reverse '(a b c))`,
		Expect: "(c b a)",
	}).Test(t)
	(&sliptest.Function{
		Source: `(reverse '(a b))`,
		Expect: "(b a)",
	}).Test(t)
	(&sliptest.Function{
		Source: `(reverse '(a))`,
		Expect: "(a)",
	}).Test(t)
	(&sliptest.Function{
		Source: `(reverse '())`,
		Expect: "nil",
	}).Test(t)
}

func TestReverseVector(t *testing.T) {
	(&sliptest.Function{
		Source: `(reverse #(a b c d))`,
		Array:  true,
		Expect: "#(d c b a)",
	}).Test(t)
	(&sliptest.Function{
		Source: `(reverse #(a))`,
		Array:  true,
		Expect: "#(a)",
	}).Test(t)
	(&sliptest.Function{
		Source: `(reverse #())`,
		Array:  true,
		Expect: "#()",
	}).Test(t)
}

func TestReverseString(t *testing.T) {
	(&sliptest.Function{
		Source: `(reverse "abcd")`,
		Expect: `"dcba"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(reverse "a")`,
		Expect: `"a"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(reverse "")`,
		Expect: `""`,
	}).Test(t)
}

func TestReverseOctets(t *testing.T) {
	(&sliptest.Function{
		Source: `(reverse (coerce "abcd" 'octets))`,
		Array:  true,
		Expect: "#(100 99 98 97)",
	}).Test(t)
	(&sliptest.Function{
		Source: `(reverse (coerce "a" 'octets))`,
		Array:  true,
		Expect: "#(97)",
	}).Test(t)
	(&sliptest.Function{
		Source: `(reverse (coerce "" 'octets))`,
		Array:  true,
		Expect: "#()",
	}).Test(t)
}

func TestReverseBitVector(t *testing.T) {
	(&sliptest.Function{
		Source: `(reverse #*1010)`,
		Expect: "#*0101",
	}).Test(t)
}

func TestReverseNotSequence(t *testing.T) {
	(&sliptest.Function{
		Source: `(reverse 7)`,
		Panics: true,
	}).Test(t)
}
