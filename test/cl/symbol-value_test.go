// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestSymbolValueNormal(t *testing.T) {
	_ = slip.ReadString("(defvar symbol-value-test 3)").Eval(slip.NewScope(), nil)
	(&sliptest.Function{
		Source: `(symbol-value 'symbol-value-test)`,
		Expect: "3",
	}).Test(t)
}

func TestSymbolValueKeyword(t *testing.T) {
	(&sliptest.Function{
		Source: `(symbol-value :xYz)`,
		Expect: `:xyz`,
	}).Test(t)
}

func TestSymbolValueNotSymbol(t *testing.T) {
	(&sliptest.Function{
		Source:    `(symbol-value t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestSymbolValueNotBound(t *testing.T) {
	(&sliptest.Function{
		Source:    `(symbol-value 'sym-val-unbound)`,
		PanicType: slip.UnboundVariableSymbol,
	}).Test(t)
}
