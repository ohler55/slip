// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestSymbolFunctionNormal(t *testing.T) {
	(&sliptest.Function{
		Source: `(symbol-function 'car)`,
		Expect: "#<built-in car>",
	}).Test(t)
}

func TestSymbolFunctionNotSymbol(t *testing.T) {
	(&sliptest.Function{
		Source:    `(symbol-function t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestSymbolFunctionNotBound(t *testing.T) {
	(&sliptest.Function{
		Source:    `(symbol-function 'sym-fun-unbound)`,
		PanicType: slip.UndefinedFunctionSymbol,
	}).Test(t)
}
