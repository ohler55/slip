// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

import "fmt"

// DivisionByZeroSymbol is the symbol with a value of "division-by-zero".
const DivisionByZeroSymbol = Symbol("division-by-zero")

// NewDivisionByZero creates a DivisionByZeroPanic (arithmetic-error) describing a arithmetic error.
func NewDivisionByZero(s *Scope, depth int, operation Object, operands List, format string, args ...any) Object {
	c := FindClass("division-by-zero")
	obj := c.MakeInstance()

	obj.Init(s, List{
		Symbol(":operation"), operation,
		Symbol(":operands"), operands,
		Symbol(":message"), String(fmt.Sprintf(format, args...)),
	}, depth)
	return obj
}

// PanicDivisionByZero raises a DivisionByZeroPanic (division-by-zero)
// describing a division by zero error.
func PanicDivisionByZero(s *Scope, depth int, operation Object, operands List, format string, args ...any) {
	panic(NewDivisionByZero(s, depth, operation, operands, format, args...))
}
