// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestCharEqTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(char= #\A)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(char= #\A #\A)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(char= #\π #\π)`,
		Expect: "t",
	}).Test(t)
}

func TestCharEqFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(char= #\A #\a)`,
		Expect: "nil",
	}).Test(t)
}

func TestCharEqNotCharacter(t *testing.T) {
	(&sliptest.Function{
		Source: `(char= 7)`,
		Panics: true,
	}).Test(t)
}
