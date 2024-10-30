// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestRevappendEmpty(t *testing.T) {
	(&sliptest.Function{
		Source: `(revappend '() '())`,
		Expect: "nil",
	}).Test(t)
}

func TestRevappendOne(t *testing.T) {
	(&sliptest.Function{
		Source: `(revappend '(a) '(b))`,
		Expect: "(a b)",
	}).Test(t)
}

func TestRevappendMultiple(t *testing.T) {
	(&sliptest.Function{
		Source: `(revappend '(c b a) '(d e))`,
		Expect: "(a b c d e)",
	}).Test(t)
}

func TestRevappendTail(t *testing.T) {
	(&sliptest.Function{
		Source: `(revappend '(b a) 'c)`,
		Expect: "(a b . c)",
	}).Test(t)
}

func TestRevappendNotList(t *testing.T) {
	(&sliptest.Function{
		Source:    `(revappend t '(d e))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
