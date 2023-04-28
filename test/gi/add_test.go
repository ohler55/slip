// Copyright (c) 2023, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestAddEmpty(t *testing.T) {
	(&sliptest.Function{
		Source: `(add nil)`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: `(add '())`,
		Expect: "nil",
	}).Test(t)
}

func TestAddBasic(t *testing.T) {
	(&sliptest.Function{
		Source: `(add nil 'a 'b)`,
		Expect: "(a b)",
	}).Test(t)
	(&sliptest.Function{
		Source: `(add '(a b) 'c)`,
		Expect: "(a b c)",
	}).Test(t)
}

func TestAddNotList(t *testing.T) {
	(&sliptest.Function{
		Source: `(add t)`,
		Panics: true,
	}).Test(t)
}
