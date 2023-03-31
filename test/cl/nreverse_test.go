// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestNreverseNil(t *testing.T) {
	(&sliptest.Function{
		Source: `(nreverse nil)`,
		Expect: "nil",
	}).Test(t)
}

func TestNreverseList(t *testing.T) {
	(&sliptest.Function{
		Source: `(nreverse '(a b c d))`,
		Expect: "(d c b a)",
	}).Test(t)
	(&sliptest.Function{
		Source: `(nreverse '(a b c))`,
		Expect: "(c b a)",
	}).Test(t)
	(&sliptest.Function{
		Source: `(nreverse '(a b))`,
		Expect: "(b a)",
	}).Test(t)
	(&sliptest.Function{
		Source: `(nreverse '(a))`,
		Expect: "(a)",
	}).Test(t)
}

func TestNreverseVector(t *testing.T) {
	(&sliptest.Function{
		Source: `(nreverse #(a b c d))`,
		Expect: "#(d c b a)",
	}).Test(t)
}

func TestNreverseString(t *testing.T) {
	(&sliptest.Function{
		Source: `(nreverse "abcd")`,
		Expect: `"dcba"`,
	}).Test(t)
}

func TestNreverseNotSequence(t *testing.T) {
	(&sliptest.Function{
		Source: `(nreverse 7)`,
		Panics: true,
	}).Test(t)
}
