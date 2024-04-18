// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestLogiorFixnum(t *testing.T) {
	(&sliptest.Function{
		Source: `(logior 4 2 1)`,
		Expect: "7",
	}).Test(t)
}

func TestLogiorBignum(t *testing.T) {
	(&sliptest.Function{
		Source: `(format nil "~X" (logior #x11111111111111111111 #x282828282828282828))`,
		Expect: `"11393939393939393939"`,
	}).Test(t)
}

func TestLogiorMixed(t *testing.T) {
	(&sliptest.Function{
		Source: `(format nil "~X" (logior #x12345678 #x11111111111111111111 #x282828282828282828))`,
		Expect: `"1139393939393b3d7f79"`,
	}).Test(t)
}

func TestLogiorNoArgs(t *testing.T) {
	(&sliptest.Function{
		Source: `(logior)`,
		Expect: "0",
	}).Test(t)
}

func TestLogiorNotInteger(t *testing.T) {
	(&sliptest.Function{
		Source:    `(logior 1 t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(logior #x77777777777777777777 t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
