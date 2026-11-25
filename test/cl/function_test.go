// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
)

func TestFunctionSymbol(t *testing.T) {
	scope := slip.NewScope()
	tt.Equal(t, "#<built-in car>", slip.ObjectString(slip.ReadEvalString("(function car)", scope)))
}

func TestFunctionLambda(t *testing.T) {
	scope := slip.NewScope()
	tt.Equal(t, `/^#<function \(lambda \(x\)\) \{[0-9a-h]+\}>$/`, slip.ObjectString(slip.ReadEvalString("(function (lambda (x) nil))", scope)))
}

func TestFunctionSharpQuote(t *testing.T) {
	scope := slip.NewScope()
	code := slip.ReadString("#'car", scope)
	tt.Equal(t, "#<built-in car>", slip.ObjectString(code.Eval(scope, nil)))
	tt.Equal(t, "#'car", slip.ObjectString(code[0]))
}

func TestFunctionArgCount(t *testing.T) {
	scope := slip.NewScope()
	tt.Panic(t, func() { _ = slip.ReadEvalString("(function)", scope) })
}

func TestFunctionNotFunc(t *testing.T) {
	scope := slip.NewScope()
	tt.Panic(t, func() { _ = slip.ReadEvalString("(function t)", scope) })
}
