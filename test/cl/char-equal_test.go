// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestCharEqualTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(char-equal #\A)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(char-equal #\A #\a)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(char-equal #\π #\Π)`,
		Expect: "t",
	}).Test(t)
}

func TestCharEqualFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(char-equal #\A #\a #\B)`,
		Expect: "nil",
	}).Test(t)
}

func TestCharEqualNotCharacter(t *testing.T) {
	(&sliptest.Function{
		Source: `(char-equal 7)`,
		Panics: true,
	}).Test(t)
}
