// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestMakeSymbolOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(make-symbol "abc")`,
		Expect: "abc",
	}).Test(t)
}

func TestMakeSymbolNotString(t *testing.T) {
	(&sliptest.Function{
		Source: `(make-symbol 123)`,
		Panics: true,
	}).Test(t)
}
