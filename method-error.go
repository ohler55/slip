// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

import (
	"fmt"
)

// MethodErrorSymbol is the symbol with a value of "invalid-method-error".
const MethodErrorSymbol = Symbol("method-error")

// NewMethodError raises a MethodPanic (invalid-method-error)
// describing a invalid-method-error error.
func NewMethodError(class, qualifier, name Object, format string, args ...any) Object {
	c := FindClass("method-error")
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

// PanicMethodError raises a MethodPanic (invalid-method-error)
// describing a invalid-method-error error.
func PanicMethod(class, qualifier, name Object, format string, args ...any) {
	panic(NewMethodError(class, qualifier, name, format, args...))
}
