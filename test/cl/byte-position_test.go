// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestBytePositionOk(t *testing.T) {
	(&sliptest.Function{
		Source: "(byte-position '(16 . 7))",
		Expect: "7",
	}).Test(t)
}

func TestBytePositionBadSpec(t *testing.T) {
	(&sliptest.Function{
		Source:    "(byte-position t)",
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    "(byte-position '(10 . t))",
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    "(byte-position '(10 7))",
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
