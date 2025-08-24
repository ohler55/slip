// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

import "fmt"

// UndefinedFunctionSymbol is the symbol with a value of "undefined-function".
const UndefinedFunctionSymbol = Symbol("undefined-function")

// UndefinedFunctionNew returns a new UndefinedFunction (undefined-function)
// describing a undefined-function error.
func UndefinedFunctionNew(s *Scope, depth int, name Object, format string, args ...any) Object {
	c := FindClass("undefined-function")
	obj := c.MakeInstance()

	obj.Init(s, List{
		Symbol(":name"), name,
		Symbol(":message"), String(fmt.Sprintf(format, args...)),
	}, depth)
	return obj
}

// UndefinedFunctionPanic raises a UndefinedFunction (undefined-function)
// describing a undefined-function error.
func UndefinedFunctionPanic(s *Scope, depth int, name Object, format string, args ...any) {
	panic(UndefinedFunctionNew(s, depth, name, format, args...))
}
