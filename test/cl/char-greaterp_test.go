// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestCharGreaterpTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(char-greaterp #\A)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(char-greaterp #\C #\b #\A)`,
		Expect: "t",
	}).Test(t)
}

func TestCharGreaterpFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(char-greaterp #\A #\A)`,
		Expect: "nil",
	}).Test(t)
}

func TestCharGreaterpNotCharacter(t *testing.T) {
	(&sliptest.Function{
		Source: `(char-greaterp 7)`,
		Panics: true,
	}).Test(t)
}
