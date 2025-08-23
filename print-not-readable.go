// Copyright (c) 2025, Peter Ohler, All rights reserved.

package slip

import "fmt"

// PrintNotReadableSymbol is the symbol with a value of "print-not-readable".
const PrintNotReadableSymbol = Symbol("print-not-readable")

// PrintNotReadableNew creates a print-not-readable error.
func PrintNotReadableNew(s *Scope, depth int, object Object, format string, args ...any) Object {
	c := FindClass("print-not-readable")
	obj := c.MakeInstance()
	obj.Init(s, List{
		Symbol(":object"), object,
		Symbol(":message"), String(fmt.Sprintf(format, args...)),
	}, depth)
	return obj
}

// PrintNotReadablePanic raises a print-not-readable.
func PrintNotReadablePanic(s *Scope, depth int, object Object, format string, args ...any) {
	panic(PrintNotReadableNew(s, depth, object, format, args...))
}
