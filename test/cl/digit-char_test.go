// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestDigitCharOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(digit-char 7)`,
		Expect: `#\7`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(digit-char 35 36)`,
		Expect: `#\Z`,
	}).Test(t)
}

func TestDigitCharNil(t *testing.T) {
	(&sliptest.Function{
		Source: `(digit-char 11)`,
		Expect: "nil",
	}).Test(t)
}

func TestDigitCharNotInteger(t *testing.T) {
	(&sliptest.Function{
		Source: `(digit-char t)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(digit-char 7 t)`,
		Panics: true,
	}).Test(t)
}

func TestDigitCharOutOfRange(t *testing.T) {
	(&sliptest.Function{
		Source: `(digit-char -7)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(digit-char 11)`,
		Expect: "nil",
	}).Test(t)
}
