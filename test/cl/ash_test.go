// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestAshFixnum(t *testing.T) {
	(&sliptest.Function{
		Source: `(ash 1 2)`,
		Expect: "4",
	}).Test(t)
	(&sliptest.Function{
		Source: `(ash 4 -2)`,
		Expect: "1",
	}).Test(t)
}

func TestAshOctet(t *testing.T) {
	(&sliptest.Function{
		Source: `(ash (coerce 1 'octet) 2)`,
		Expect: "4",
	}).Test(t)
	(&sliptest.Function{
		Source: `(ash (coerce 4 'octet) -2)`,
		Expect: "1",
	}).Test(t)
}

func TestAshBignum(t *testing.T) {
	(&sliptest.Function{
		Source: `(format nil "~X" (ash #x808080808080808080 -1))`,
		Expect: `"404040404040404040"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~X" (ash #x818181818181818181 -1))`,
		Expect: `"40c0c0c0c0c0c0c0c0"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~X" (ash #x100000000000000000 -42))`,
		Expect: `"4000000"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~X" (ash (- 0 #x808080808080808080) -1))`,
		Expect: `"-404040404040404040"`,
	}).Test(t)

	(&sliptest.Function{
		Source: `(format nil "~X" (ash #x404040404040404040 1))`,
		Expect: `"808080808080808080"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~X" (ash #x808080808080808080 1))`,
		Expect: `"1010101010101010100"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~X" (ash #x000000000000000100 42))`,
		Expect: `"4000000000000"`,
	}).Test(t)
}

func TestAshNotInteger(t *testing.T) {
	(&sliptest.Function{
		Source:    `(ash t 1)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(ash 1 t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
