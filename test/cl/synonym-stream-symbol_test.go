// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestSynonymStreamSymbolOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(synonym-stream-symbol (make-synonym-stream 'zz))`,
		Expect: "zz",
	}).Test(t)
}

func TestSynonymStreamSymbolNotSymbol(t *testing.T) {
	(&sliptest.Function{
		Source:    `(synonym-stream-symbol t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
