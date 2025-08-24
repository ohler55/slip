// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

import "fmt"

// ArithmeticErrorSymbol is the symbol with a value of "arithmetic-error".
const ArithmeticErrorSymbol = Symbol("arithmetic-error")

// ArithmeticErrorNew creates a ArithmeticError (arithmetic-error) describing
// a arithmetic error.
func ArithmeticErrorNew(s *Scope, depth int, operation Object, operands List, format string, args ...any) Object {
	c := FindClass("arithmetic-error")
	obj := c.MakeInstance()

	obj.Init(s, List{
		Symbol(":operation"), operation,
		Symbol(":operands"), operands,
		Symbol(":message"), String(fmt.Sprintf(format, args...)),
	}, depth)
	return obj
}

// ArithmeticPanic raises a ArithmeticError (arithmetic-error) describing a
// arithmetic error.
func ArithmeticPanic(s *Scope, depth int, operation Object, operands List, format string, args ...any) {
	panic(ArithmeticErrorNew(s, depth, operation, operands, format, args...))
}
