// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestEndpEmpty(t *testing.T) {
	(&sliptest.Function{
		Source: `(endp nil)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(endp '())`,
		Expect: "t",
	}).Test(t)
}

func TestEndpNotEmoty(t *testing.T) {
	(&sliptest.Function{
		Source: `(endp '(a b c))`,
		Expect: "nil",
	}).Test(t)
}

func TestEndpNotList(t *testing.T) {
	(&sliptest.Function{
		Source:    `(endp t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
