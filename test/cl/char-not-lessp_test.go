// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestCharNotLesspTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(char-not-lessp #\A)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(char-not-lessp #\C #\b #\B #\A)`,
		Expect: "t",
	}).Test(t)
}

func TestCharNotLesspFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(char-not-lessp #\A #\B)`,
		Expect: "nil",
	}).Test(t)
}

func TestCharNotLesspNotCharacter(t *testing.T) {
	(&sliptest.Function{
		Source: `(char-not-lessp 7)`,
		Panics: true,
	}).Test(t)
}
