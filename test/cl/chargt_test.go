// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestCharGtTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(char> #\A)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(char> #\C #\B #\A)`,
		Expect: "t",
	}).Test(t)
}

func TestCharGtFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(char> #\A #\A)`,
		Expect: "nil",
	}).Test(t)
}

func TestCharGtNotCharacter(t *testing.T) {
	(&sliptest.Function{
		Source: `(char> 7)`,
		Panics: true,
	}).Test(t)
}
