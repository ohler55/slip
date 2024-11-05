// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestProgvEmpty(t *testing.T) {
	(&sliptest.Function{
		Source: `(progv nil nil)`,
		Expect: "nil",
	}).Test(t)
}

func TestProgvBasic(t *testing.T) {
	(&sliptest.Function{
		Source: `(progv '(x) '(2) (1+ x))`,
		Expect: "3",
	}).Test(t)
}

func TestProgvUnbound(t *testing.T) {
	(&sliptest.Function{
		Source: `(progv '(x) '() (boundp 'x))`,
		Expect: "nil",
	}).Test(t)
}

func TestProgvBadSymbols(t *testing.T) {
	(&sliptest.Function{
		Source:    `(progv t '() (boundp 'x))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(progv '(t) '() (boundp 'x))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestProgvBadValues(t *testing.T) {
	(&sliptest.Function{
		Source:    `(progv '() t (boundp 'x))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
