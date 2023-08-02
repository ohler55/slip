// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestCharNameAlpha(t *testing.T) {
	(&sliptest.Function{
		Source: `(char-name #\A)`,
		Expect: `"Capital-A"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(char-name #\a)`,
		Expect: `"Small-A"`,
	}).Test(t)
}

func TestCharNameNonGraphic(t *testing.T) {
	(&sliptest.Function{
		Source: `(char-name (code-char 9))`,
		Expect: `"Tab"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(char-name (code-char 21))`,
		Expect: `"NAK"`,
	}).Test(t)
}

func TestCharNameHiBit(t *testing.T) {
	(&sliptest.Function{
		Source: `(char-name #\π)`,
		Expect: `"#\π"`,
	}).Test(t)
}

func TestCharNameNotChar(t *testing.T) {
	(&sliptest.Function{
		Source: `(char-name 7)`,
		Panics: true,
	}).Test(t)
}
