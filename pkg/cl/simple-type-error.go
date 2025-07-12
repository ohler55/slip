// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import "github.com/ohler55/slip"

// SimpleTypeErrorSymbol is the symbol with a value of "simple-error".
const SimpleTypeErrorSymbol = slip.Symbol("simple-type-error")

// NewSimpleTypeError returns a SimpleTypeErrorObj object that can then be used with a call to panic.
func NewSimpleTypeError(s *slip.Scope, ctrl string, args ...slip.Object) slip.Object {
	c := slip.FindClass("simple-type-error")
	obj := c.MakeInstance()
	argList := make(slip.List, 0, len(args))
	argList = append(argList, args...)
	obj.Init(s, slip.List{
		slip.Symbol(":format-control"), slip.String(ctrl),
		slip.Symbol(":format-arguments"), argList,
	}, 0)

	return obj
}

// PanicSimpleTypeError raises a SimpleTypeErrorObj instance.
func PanicSimpleTypeError(s *slip.Scope, ctrl string, args ...slip.Object) {
	panic(NewSimpleTypeError(s, ctrl, args...))
}
