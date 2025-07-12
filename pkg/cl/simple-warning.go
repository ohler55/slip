// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import "github.com/ohler55/slip"

// SimpleWarningSymbol is the symbol with a value of "simple-error".
const SimpleWarningSymbol = slip.Symbol("simple-warning")

// NewSimpleWarning returns a SimpleWarningObj object that can then be used with a call to panic.
func NewSimpleWarning(s *slip.Scope, ctrl string, args ...slip.Object) slip.Object {
	c := slip.FindClass("simple-warning")
	obj := c.MakeInstance()
	argList := make(slip.List, 0, len(args))
	argList = append(argList, args...)
	obj.Init(s, slip.List{
		slip.Symbol(":format-control"), slip.String(ctrl),
		slip.Symbol(":format-arguments"), argList,
	}, 0)

	return obj
}

// PanicSimpleWarning raises a SimpleErrorObj instance.
func PanicSimpleWarning(s *slip.Scope, ctrl string, args ...slip.Object) {
	panic(NewSimpleWarning(s, ctrl, args...))
}
