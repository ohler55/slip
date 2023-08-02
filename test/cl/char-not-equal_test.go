// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestCharNotEqualTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(char-not-equal #\A)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(char-not-equal #\A #\b #\C)`,
		Expect: "t",
	}).Test(t)
}

func TestCharNotEqualFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(char-not-equal #\A #\b #\a)`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: `(char-not-equal #\π #\Π)`,
		Expect: "nil",
	}).Test(t)
}

func TestCharNotEqualNotCharacter(t *testing.T) {
	(&sliptest.Function{
		Source: `(char-not-equal 7)`,
		Panics: true,
	}).Test(t)
}
