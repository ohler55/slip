// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestDoSymbolsSimple(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((lst ())) (do-symbols (s *bag* lst)))`,
		Expect: "nil",
	}).Test(t)
}
