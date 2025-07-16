// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

import (
	"fmt"
)

// ErrorSymbol is the symbol with a value of "error".
const ErrorSymbol = Symbol("error")

// NewError returns a Panic object that can then be used with a call to panic.
func NewError(format string, args ...any) Object {
	c := FindClass("error")
	obj := c.MakeInstance()
	obj.Init(NewScope(), List{
		Symbol(":message"), String(fmt.Sprintf(format, args...)),
	}, 0)
	return obj
}

// PanicError raises an error.
func PanicError(format string, args ...any) {
	panic(NewError(format, args...))
}
