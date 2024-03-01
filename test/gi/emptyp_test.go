// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestEmptypTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(emptyp "")`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(emptyp nil)`,
		Expect: "t",
	}).Test(t)
}

func TestEmptypFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(emptyp "abc")`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: `(emptyp 7)`,
		Expect: "nil",
	}).Test(t)
}
