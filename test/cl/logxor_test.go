// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestLogxorFixnum(t *testing.T) {
	(&sliptest.Function{
		Source: `(logxor 4 7 1)`,
		Expect: "2",
	}).Test(t)
}

func TestLogxorBignum(t *testing.T) {
	(&sliptest.Function{
		Source: `(format nil "~X" (logxor #x33333333333333333333 #x383838383838383838))`,
		Expect: `"330b0b0b0b0b0b0b0b0b"`,
	}).Test(t)
}

func TestLogxorMixed(t *testing.T) {
	(&sliptest.Function{
		Source: `(format nil "~X" (logxor #x12345678 #x33333333333333333333 #x383838383838383838))`,
		Expect: `"330b0b0b0b0b193f5d73"`,
	}).Test(t)
}

func TestLogxorNoArgs(t *testing.T) {
	(&sliptest.Function{
		Source: `(logxor)`,
		Expect: "0",
	}).Test(t)
}

func TestLogxorNotInteger(t *testing.T) {
	(&sliptest.Function{
		Source:    `(logxor 1 t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(logxor #x77777777777777777777 t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
