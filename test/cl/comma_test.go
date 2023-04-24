// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestCommaNormal(t *testing.T) {
	scope := slip.NewScope()
	scope.Let("x", slip.Fixnum(2))
	(&sliptest.Function{
		Scope:  scope,
		Source: "`(+ 1 ,x)",
		Expect: "(+ 1 2)",
	}).Test(t)
}
