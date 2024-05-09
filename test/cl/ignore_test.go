// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestIgnore(t *testing.T) {
	(&sliptest.Function{
		Source:    `(ignore 'abc)`,
		PanicType: slip.UndefinedFunctionSymbol,
	}).Test(t)
}
