// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestLogeqvFixnum(t *testing.T) {
	(&sliptest.Function{
		Source: `(logeqv 7 3 1)`,
		Expect: "5",
	}).Test(t)
	(&sliptest.Function{
		Source: `(logeqv 7 3)`,
		Expect: "-5",
	}).Test(t)
}

func TestLogeqvBignum(t *testing.T) {
	(&sliptest.Function{
		Source: `(format nil "~X" (logeqv #x333333333333333333 #x383838383838383838))`,
		Expect: `"f4f4f4f4f4f4f4f4f4"`, // same as -B0B0B0B0B0B0B0B0C
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~X" (logeqv #x33333333333333333333 #x383838383838383838))`,
		Expect: `"ccf4f4f4f4f4f4f4f4f4"`, // same as -330B0B0B0B0B0B0B0B0C
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~X" (logeqv #x333333333333333333 #x38383838383838383838))`,
		Expect: `"c7f4f4f4f4f4f4f4f4f4"`, // same as -380B0B0B0B0B0B0B0B0C
	}).Test(t)
}

func TestLogeqvMixed(t *testing.T) {
	(&sliptest.Function{
		Source: `(format nil "~X" (logeqv #x12345678 #x33333333333333333333 #x383838383838383838))`,
		Expect: `"330b0b0b0b0b193f5d73"`,
	}).Test(t)
}

func TestLogeqvNoArgs(t *testing.T) {
	(&sliptest.Function{
		Source: `(logeqv)`,
		Expect: "-1",
	}).Test(t)
}

func TestLogeqvNotInteger(t *testing.T) {
	(&sliptest.Function{
		Source:    `(logeqv 1 t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(logeqv #x77777777777777777777 t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
