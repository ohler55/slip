// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestGraphicCharPTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(graphic-char-p #\A)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(graphic-char-p #\π)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(graphic-char-p #\space)`,
		Expect: "t",
	}).Test(t)
}

func TestGraphicCharPFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(graphic-char-p #\newline)`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: `(graphic-char-p #\tab)`,
		Expect: "nil",
	}).Test(t)
}

func TestGraphicCharPNotCharacter(t *testing.T) {
	(&sliptest.Function{
		Source: `(graphic-char-p 7)`,
		Panics: true,
	}).Test(t)
}
