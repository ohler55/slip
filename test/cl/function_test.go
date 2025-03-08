// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
)

func TestFunctionSymbol(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString("(function car)", scope)
	tt.Equal(t, "#<function car>", slip.ObjectString(code.Eval(scope, nil)))
}

func TestFunctionLambda(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString("(function (lambda (x) nil))", scope)
	tt.Equal(t, `/^#<function \(lambda \(x\)\) \{[0-9a-h]+\}>$/`, slip.ObjectString(code.Eval(scope, nil)))
}

func TestFunctionSharpQuote(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString("#'car", scope)
	tt.Equal(t, "#<function car>", slip.ObjectString(code.Eval(scope, nil)))
	tt.Equal(t, "#'car", slip.ObjectString(code[0]))
}

func TestFunctionArgCount(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString("(function)", scope)
	tt.Panic(t, func() { _ = code.Eval(scope, nil) })
}

func TestFunctionNotFunc(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString("(function t)", scope)
	tt.Panic(t, func() { _ = code.Eval(scope, nil) })
}
