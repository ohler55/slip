// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

import "fmt"

// ClassNotFoundSymbol is the symbol with a value of "unbound-slot".
const ClassNotFoundSymbol = Symbol("class-not-found")

// NewClassNotFound creates a new ClassNotFoundPanic (class-not-found)
// describing a class-not-found error.
func NewClassNotFound(name Object, format string, args ...any) Object {
	c := FindClass("class-not-found")
	obj := c.MakeInstance()

	obj.Init(NewScope(), List{
		Symbol(":name"), name,
		Symbol(":message"), String(fmt.Sprintf(format, args...)),
	}, 0)
	return obj
}

// PanicClassNotFound raises a ClassNotFoundPanic (unbound-slot)
// describing a unbound-slot error.
func PanicClassNotFound(name Object, format string, args ...any) {
	panic(NewClassNotFound(name, format, args...))
}
