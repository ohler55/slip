// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

import (
	"fmt"
)

// InvalidMethodErrorSymbol is the symbol with a value of "invalid-method-error".
const InvalidMethodErrorSymbol = Symbol("invalid-method-error")

// NewInvalidMethodError raises a MethodPanic (invalid-method-error)
// describing a invalid-method-error error.
func NewInvalidMethodError(class, qualifier, name Object, format string, args ...any) Object {
	c := FindClass("invalid-method-error")
	obj := c.MakeInstance()

	argList := List{
		Symbol(":class"), class,
		Symbol(":qualifier"), qualifier,
		Symbol(":method"), name,
	}
	if 0 < len(format) {
		argList = append(argList, Symbol(":message"), String(fmt.Sprintf(format, args...)))
	} else {
		argList = append(argList,
			Symbol(":message"),
			String(fmt.Sprintf("%s %s is not a valid method combination for %s.", qualifier, name, class)))
	}
	obj.Init(NewScope(), argList, 0)

	return obj
}

// PanicInvalidMethodError raises a MethodPanic (invalid-method-error)
// describing a invalid-method-error error.
func PanicInvalidMethod(class, qualifier, name Object, format string, args ...any) {
	panic(NewInvalidMethodError(class, qualifier, name, format, args...))
}
