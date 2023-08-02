// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestCharLesspTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(char-lessp #\A)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(char-lessp #\A #\b #\C)`,
		Expect: "t",
	}).Test(t)
}

func TestCharLesspFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(char-lessp #\B #\A)`,
		Expect: "nil",
	}).Test(t)
}

func TestCharLesspNotCharacter(t *testing.T) {
	(&sliptest.Function{
		Source: `(char-lessp 7)`,
		Panics: true,
	}).Test(t)
}
