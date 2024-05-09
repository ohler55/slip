// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestFtype(t *testing.T) {
	(&sliptest.Function{
		Source:    `(ftype 'abc)`,
		PanicType: slip.UndefinedFunctionSymbol,
	}).Test(t)
}
