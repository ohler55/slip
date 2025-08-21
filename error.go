// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

import (
	"fmt"
)

// ErrorSymbol is the symbol with a value of "error".
const ErrorSymbol = Symbol("error")

// NewError returns a Panic object that can then be used with a call to panic.
func NewError(format string, args ...any) Object {
	return ErrorNew(NewScope(), 0, format, args...)
}

// PanicError raises an error.
func PanicError(format string, args ...any) {
	// func PanicError(s *Scope, depth int, format string, args ...any) {
	// panic(NewError(s, depth, format, args...))
	panic(NewError(format, args...))
}

// ErrorNew returns an Error object that can then be used with a call to panic.
func ErrorNew(s *Scope, depth int, format string, args ...any) Object {
	c := FindClass("error")
	obj := c.MakeInstance()
	obj.Init(s, List{
		Symbol(":message"), String(fmt.Sprintf(format, args...)),
	}, depth)
	return obj
}

// ErrorPanic raises an error.
func ErrorPanic(s *Scope, depth int, format string, args ...any) {
	// panic(NewError(s, depth, format, args...))
	panic(ErrorNew(s, depth, format, args...))
}
