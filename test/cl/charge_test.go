// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestCharGeTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(char>= #\A)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(char>= #\C #\B #\B #\A)`,
		Expect: "t",
	}).Test(t)
}

func TestCharGeFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(char>= #\A #\B)`,
		Expect: "nil",
	}).Test(t)
}

func TestCharGeNotCharacter(t *testing.T) {
	(&sliptest.Function{
		Source: `(char>= 7)`,
		Panics: true,
	}).Test(t)
}
