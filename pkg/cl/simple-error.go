// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import "github.com/ohler55/slip"

// SimpleErrorSymbol is the symbol with a value of "simple-error".
const SimpleErrorSymbol = slip.Symbol("simple-error")

// NewSimpleError returns a simple-error object that can then be used with a
// call to panic.
func NewSimpleError(s *slip.Scope, ctrl string, args slip.List) slip.Object {
	c := slip.FindClass("simple-error")
	obj := c.MakeInstance()
	obj.Init(s, slip.List{
		slip.Symbol(":format-control"), slip.String(ctrl),
		slip.Symbol(":format-arguments"), args,
	}, 0)

	return obj
}

// PanicSimpleError raises a SimpleErrorObj instance.
func PanicSimpleError(s *slip.Scope, ctrl string, args slip.List) {
	panic(NewSimpleError(s, ctrl, args))
}
