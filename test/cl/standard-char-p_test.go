// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestStandardCharPTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(standard-char-p #\A)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(standard-char-p #\7)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(standard-char-p #\space)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(standard-char-p #\newline)`,
		Expect: "t",
	}).Test(t)
}

func TestStandardCharPFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(standard-char-p #\π)`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: `(standard-char-p #\tab)`,
		Expect: "nil",
	}).Test(t)
}

func TestStandardCharPNotCharacter(t *testing.T) {
	(&sliptest.Function{
		Source: `(standard-char-p 7)`,
		Panics: true,
	}).Test(t)
}
