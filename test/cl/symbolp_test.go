// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestSymbolpTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(symbolp 'abc)`,
		Expect: "t",
	}).Test(t)
}

func TestSymbolpFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(symbolp t)`,
		Expect: "nil",
	}).Test(t)
}

func TestSymbolpBadArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(symbolp)`,
		Panics: true,
	}).Test(t)
}
