// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestAlphaCharPTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(alpha-char-p #\A)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(alpha-char-p #\π)`,
		Expect: "t",
	}).Test(t)
}

func TestAlphaCharPFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(alpha-char-p #\7)`,
		Expect: "nil",
	}).Test(t)
}

func TestAlphaCharPNotCharacter(t *testing.T) {
	(&sliptest.Function{
		Source: `(alpha-char-p 7)`,
		Panics: true,
	}).Test(t)
}
