// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestCharLtTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(char< #\A)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(char< #\A #\B #\C)`,
		Expect: "t",
	}).Test(t)
}

func TestCharLtFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(char< #\B #\A)`,
		Expect: "nil",
	}).Test(t)
}

func TestCharLtNotCharacter(t *testing.T) {
	(&sliptest.Function{
		Source: `(char< 7)`,
		Panics: true,
	}).Test(t)
}
