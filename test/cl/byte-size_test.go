// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestByteSizeOk(t *testing.T) {
	(&sliptest.Function{
		Source: "(byte-size '(16 . 7))",
		Expect: "16",
	}).Test(t)
}

func TestByteSizeBadSpec(t *testing.T) {
	(&sliptest.Function{
		Source:    "(byte-size t)",
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    "(byte-size '(t . 7))",
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
