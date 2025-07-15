// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

import "fmt"

// ArithmeticErrorSymbol is the symbol with a value of "arithmetic-error".
const ArithmeticErrorSymbol = Symbol("arithmetic-error")

// NewArithmeticError creates a ArithmeticPanic (arithmetic-error) describing a arithmetic error.
func NewArithmeticError(operation Object, operands List, format string, args ...any) Object {
	c := FindClass("arithmetic-error")
	obj := c.MakeInstance()

	obj.Init(NewScope(), List{
		Symbol(":operation"), operation,
		Symbol(":operands"), operands,
		Symbol(":message"), String(fmt.Sprintf(format, args...)),
	}, 0)
	return obj
}

// PanicArithmetic raises a ArithmeticPanic (arithmetic-error) describing a arithmetic
// error.
func PanicArithmetic(operation Object, operands List, format string, args ...any) {
	panic(NewArithmeticError(operation, operands, format, args...))
}
