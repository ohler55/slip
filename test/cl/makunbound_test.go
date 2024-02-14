// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestMakunboundOk(t *testing.T) {
	scope := slip.NewScope()
	scope.Set("makunbound-test", slip.Fixnum(5))
	(&sliptest.Function{
		Scope:  scope,
		Source: `(makunbound 'makunbound-test)`,
		Expect: "makunbound-test",
	}).Test(t)
	result := slip.ReadString("(boundp 'makunbound-test)").Eval(slip.NewScope(), nil)
	tt.Equal(t, nil, result)
}

func TestMakunboundNotSymbol(t *testing.T) {
	(&sliptest.Function{
		Source:    `(makunbound 7)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}
