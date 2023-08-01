// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestCharNotGreaterpTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(char-not-greaterp #\A)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(char-not-greaterp #\A #\b #\B #\C)`,
		Expect: "t",
	}).Test(t)
}

func TestCharNotGreaterpFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(char-not-greaterp #\B #\A)`,
		Expect: "nil",
	}).Test(t)
}

func TestCharNotGreaterpNotCharacter(t *testing.T) {
	(&sliptest.Function{
		Source: `(char-not-greaterp 7)`,
		Panics: true,
	}).Test(t)
}
