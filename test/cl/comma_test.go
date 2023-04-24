// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/cl"
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

func TestCommaList(t *testing.T) {
	scope := slip.NewScope()
	scope.Let("x", slip.Fixnum(2))
	(&sliptest.Function{
		Scope:  scope,
		Source: "`(+ 1 ,(+ 3 x))",
		Expect: "(+ 1 5)",
	}).Test(t)
}

func TestCommaArgCount(t *testing.T) {
	var comma cl.Comma
	tt.Panic(t, func() { _ = comma.Call(nil, slip.List{}, 0) })
}

func TestCommaString(t *testing.T) {
	comma := cl.Comma{Function: slip.Function{Name: "comma", Args: slip.List{slip.Symbol("x")}}}

	tt.Equal(t, ",x", comma.String())

	tt.Equal(t, ',', comma.SpecialChar())
}
