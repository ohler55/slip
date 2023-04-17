// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
)

func TestFunctionSymbol(t *testing.T) {
	code := slip.ReadString("(function car)")
	tt.Equal(t, "#<function car>", slip.ObjectString(code.Eval(slip.NewScope(), nil)))
}

func TestFunctionLambda(t *testing.T) {
	code := slip.ReadString("(function (lambda (x) nil))")
	tt.Equal(t, `/^#<function \(lambda \(x\)\) \{[0-9a-h]+\}>$/`, slip.ObjectString(code.Eval(slip.NewScope(), nil)))
}

func TestFunctionSharpQuote(t *testing.T) {
	code := slip.ReadString("#'car")
	tt.Equal(t, "#<function car>", slip.ObjectString(code.Eval(slip.NewScope(), nil)))
	tt.Equal(t, "#'car", slip.ObjectString(code[0]))
}

func TestFunctionArgCount(t *testing.T) {
	code := slip.ReadString("(function)")
	tt.Panic(t, func() { _ = code.Eval(slip.NewScope(), nil) })
}

func TestFunctionNotFunc(t *testing.T) {
	code := slip.ReadString("(function t)")
	tt.Panic(t, func() { _ = code.Eval(slip.NewScope(), nil) })
}
