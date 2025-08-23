// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

import "fmt"

// ClassNotFoundSymbol is the symbol with a value of "unbound-slot".
const ClassNotFoundSymbol = Symbol("class-not-found")

// ClassNotFoundNew creates a new ClassNotFound (class-not-found) describing a
// class-not-found error.
func ClassNotFoundNew(s *Scope, depth int, name Object, format string, args ...any) Object {
	c := FindClass("class-not-found")
	obj := c.MakeInstance()

	obj.Init(s, List{
		Symbol(":name"), name,
		Symbol(":message"), String(fmt.Sprintf(format, args...)),
	}, depth)
	return obj
}

// ClassNotFoundPanic raises a ClassNotFound (unbound-slot) describing a
// class-not-found error.
func ClassNotFoundPanic(s *Scope, depth int, name Object, format string, args ...any) {
	panic(ClassNotFoundNew(s, depth, name, format, args...))
}
