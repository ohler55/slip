// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestNreconcEmpty(t *testing.T) {
	(&sliptest.Function{
		Source: `(nreconc '() '())`,
		Expect: "nil",
	}).Test(t)
}

func TestNreconcOne(t *testing.T) {
	(&sliptest.Function{
		Source: `(nreconc '(a) '(b))`,
		Expect: "(a b)",
	}).Test(t)
}

func TestNreconcMultiple(t *testing.T) {
	(&sliptest.Function{
		Source: `(nreconc '(c b a) '(d e))`,
		Expect: "(a b c d e)",
	}).Test(t)
}

func TestNreconcTail(t *testing.T) {
	(&sliptest.Function{
		Source: `(nreconc '(b a) 'c)`,
		Expect: "(a b . c)",
	}).Test(t)
}

func TestNreconcNotList(t *testing.T) {
	(&sliptest.Function{
		Source:    `(nreconc t '(d e))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
