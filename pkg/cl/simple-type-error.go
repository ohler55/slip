// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import "github.com/ohler55/slip"

// SimpleTypeErrorSymbol is the symbol with a value of "simple-error".
const SimpleTypeErrorSymbol = slip.Symbol("simple-type-error")

// SimpleTypeErrorNew returns a SimpleTypeError object that can then be used
// with a call to panic.
func SimpleTypeErrorNew(s *slip.Scope, depth int, ctrl string, args ...slip.Object) slip.Object {
	c := slip.FindClass("simple-type-error")
	obj := c.MakeInstance()
	argList := make(slip.List, 0, len(args))
	argList = append(argList, args...)
	obj.Init(s, slip.List{
		slip.Symbol(":format-control"), slip.String(ctrl),
		slip.Symbol(":format-arguments"), argList,
	}, depth)

	return obj
}

// SimpleTypeErrorPanic raises a SimpleTypeError instance.
func SimpleTypeErrorPanic(s *slip.Scope, depth int, ctrl string, args ...slip.Object) {
	panic(SimpleTypeErrorNew(s, depth, ctrl, args...))
}
