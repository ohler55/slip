// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import "github.com/ohler55/slip"

// SimpleErrorSymbol is the symbol with a value of "simple-error".
const SimpleErrorSymbol = slip.Symbol("simple-error")

// SimpleErrorNew returns a simple-error object that can then be used with a
// call to panic.
func SimpleErrorNew(s *slip.Scope, depth int, ctrl string, args slip.List) slip.Object {
	c := slip.FindClass("simple-error")
	obj := c.MakeInstance()
	obj.Init(s, slip.List{
		slip.Symbol(":format-control"), slip.String(ctrl),
		slip.Symbol(":format-arguments"), args,
	}, depth)

	return obj
}

// SimpleErrorPanic raises a SimpleErrorObj instance.
func SimpleErrorPanic(s *slip.Scope, depth int, ctrl string, args slip.List) {
	panic(SimpleErrorNew(s, depth, ctrl, args))
}
