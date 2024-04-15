// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestSymbolNameNormal(t *testing.T) {
	(&sliptest.Function{
		Source: `(symbol-name 'aBc)`,
		Expect: `"abc"`,
	}).Test(t)
}

func TestSymbolNameKeyword(t *testing.T) {
	(&sliptest.Function{
		Source: `(symbol-name :xYz)`,
		Expect: `"xyz"`,
	}).Test(t)
}

func TestSymbolNameNotSymbol(t *testing.T) {
	(&sliptest.Function{
		Source:    `(symbol-name t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
