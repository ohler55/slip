// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/cl"
	"github.com/ohler55/slip/sliptest"
)

func TestCommaAtNormal(t *testing.T) {
	scope := slip.NewScope()
	scope.Let("x", slip.List{slip.Fixnum(2), slip.Fixnum(3)})
	(&sliptest.Function{
		Scope:  scope,
		Source: "`(+ 1 ,@x)",
		Expect: "(+ 1 2 3)",
	}).Test(t)
}

func TestCommaAtNil(t *testing.T) {
	scope := slip.NewScope()
	scope.Let("x", nil)
	(&sliptest.Function{
		Scope:  scope,
		Source: "`(+ 1 ,@x)",
		Expect: "(+ 1)",
	}).Test(t)
}

func TestCommaAtTail(t *testing.T) {
	scope := slip.NewScope()
	scope.Let("x", slip.Fixnum(7))
	(&sliptest.Function{
		Scope:  scope,
		Source: "`(+ 1 ,@x)",
		Expect: "(+ 1 . 7)",
	}).Test(t)
	(&sliptest.Function{
		Scope:  scope,
		Source: "`(+ 1 ,@x 2)",
		Panics: true,
	}).Test(t)
}

func TestCommaAtArgCount(t *testing.T) {
	var commaAt cl.CommaAt
	tt.Panic(t, func() { _ = commaAt.Call(nil, slip.List{}, 0) })
}

func TestCommaAtString(t *testing.T) {
	commaAt := cl.CommaAt{Function: slip.Function{Name: "comma-at", Args: slip.List{slip.Symbol("x")}}}

	tt.Equal(t, ",@x", commaAt.String())

	tt.Equal(t, ",@", commaAt.SpecialPrefix())
}

func TestCommaAtList(t *testing.T) {
	scope := slip.NewScope()
	scope.Let("x", slip.List{slip.Fixnum(2), slip.Fixnum(3)})
	commaAt := cl.CommaAt{Function: slip.Function{Name: "comma-at", Args: slip.List{slip.Symbol("x")}}}
	result := commaAt.Call(scope, slip.List{slip.List{slip.Fixnum(1), slip.Fixnum(2)}}, 0)
	tt.Nil(t, result.Simplify())
	tt.Equal(t, "(1 2)", result.String())
	tt.Equal(t, false, result.Equal(nil))
	tt.Equal(t, []slip.Symbol{slip.TrueSymbol}, result.Hierarchy())
	tt.Equal(t, result, result.Eval(scope, 0))
}
