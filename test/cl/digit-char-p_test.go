// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestDigitCharPOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(digit-char-p #\7)`,
		Expect: "7",
	}).Test(t)
	(&sliptest.Function{
		Source: `(digit-char-p #\z 36)`,
		Expect: "35",
	}).Test(t)
	(&sliptest.Function{
		Source: `(digit-char-p #\Z 36)`,
		Expect: "35",
	}).Test(t)
}

func TestDigitCharPNil(t *testing.T) {
	(&sliptest.Function{
		Source: `(digit-char-p #\k 16)`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: `(digit-char-p #\Space)`,
		Expect: "nil",
	}).Test(t)
}

func TestDigitCharPNotCharacter(t *testing.T) {
	(&sliptest.Function{
		Source: `(digit-char-p t)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(digit-char-p #\3 t)`,
		Panics: true,
	}).Test(t)
}
