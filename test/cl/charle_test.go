// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestCharLeTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(char<= #\A)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(char<= #\A #\B #\B #\C)`,
		Expect: "t",
	}).Test(t)
}

func TestCharLeFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(char<= #\B #\A)`,
		Expect: "nil",
	}).Test(t)
}

func TestCharLeNotCharacter(t *testing.T) {
	(&sliptest.Function{
		Source: `(char<= 7)`,
		Panics: true,
	}).Test(t)
}
