// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestByteOk(t *testing.T) {
	(&sliptest.Function{
		Source: "(byte 16 7)",
		Expect: "(16 . 7)",
	}).Test(t)
}

func TestByteBadSize(t *testing.T) {
	(&sliptest.Function{
		Source:    "(byte t 7)",
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestByteBadPosition(t *testing.T) {
	(&sliptest.Function{
		Source:    "(byte 7 t)",
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
