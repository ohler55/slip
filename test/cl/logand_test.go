// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestLogandFixnum(t *testing.T) {
	(&sliptest.Function{
		Source: `(logand 15 10 3)`,
		Expect: "2",
	}).Test(t)
}

func TestLogandBignum(t *testing.T) {
	(&sliptest.Function{
		Source: `(format nil "~X" (logand #x77777777777777777777 #x383838383838383838))`,
		Expect: `"303030303030303030"`,
	}).Test(t)
}

func TestLogandMixed(t *testing.T) {
	(&sliptest.Function{
		Source: `(format nil "~X" (logand #x12345678 #x77777777777777777777 #x383838383838383838))`,
		Expect: `"10301030"`,
	}).Test(t)
}

func TestLogandNoArgs(t *testing.T) {
	(&sliptest.Function{
		Source: `(logand)`,
		Expect: "-1",
	}).Test(t)
}

func TestLogandNotInteger(t *testing.T) {
	(&sliptest.Function{
		Source:    `(logand 1 t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(logand #x77777777777777777777 t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(logand #x1234567890abcdef0 t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
