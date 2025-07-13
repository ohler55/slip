// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

import "fmt"

// UndefinedFunctionSymbol is the symbol with a value of "undefined-function".
const UndefinedFunctionSymbol = Symbol("undefined-function")

// NewUndefinedFunction returns a new UndefinedFunctionPanic
// (undefined-function) describing a undefined-function error.
func NewUndefinedFunction(name Object, format string, args ...any) Object {
	c := FindClass("undefined-function")
	obj := c.MakeInstance()

	obj.Init(NewScope(), List{
		Symbol(":name"), name,
		Symbol(":message"), String(fmt.Sprintf(format, args...)),
	}, 0)
	return obj
}

// PanicUndefinedFunction raises a UndefinedFunctionPanic (undefined-function)
// describing a undefined-function error.
func PanicUndefinedFunction(name Object, format string, args ...any) {
	panic(NewUndefinedFunction(name, format, args...))
}
