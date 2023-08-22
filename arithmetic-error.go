// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

import "fmt"

// ArithmeticErrorSymbol is the symbol with a value of "arithmetic-error".
const ArithmeticErrorSymbol = Symbol("arithmetic-error")

var arithmeticErrorHierarchy = []Symbol{
	ArithmeticErrorSymbol,
	ErrorSymbol,
	SeriousConditionSymbol,
	ConditionSymbol,
	TrueSymbol,
}

func init() {
	RegisterCondition("arithmetic-error", makeArithmeticError)
}

// ArithmeticError is the interface for all arithmetic-errors.
type ArithmeticError interface {
	Error

	// IsArithmeticError need not do anything other than exist.
	IsArithmeticError()

	// Operation that caused the error.
	Operation() Object

	// Operands to the function that caused the error.
	Operands() List
}

// ArithmeticPanic represents a arithmetic-error.
type ArithmeticPanic struct {
	Panic
	operation Object
	operands  List
}

// IsArithmeticError need not do anything other than exist.
func (ap *ArithmeticPanic) IsArithmeticError() {
}

// Operation that caused the error.
func (ap *ArithmeticPanic) Operation() Object {
	return ap.operation
}

// Operands to the function that caused the error.
func (ap *ArithmeticPanic) Operands() List {
	return ap.operands
}

// Equal returns true if this Object and the other are equal in value.
func (ap *ArithmeticPanic) Equal(other Object) bool {
	return ap == other
}

// Eval the object.
func (ap *ArithmeticPanic) Eval(s *Scope, depth int) Object {
	return ap
}

// NewArithmeticError creates a ArithmeticPanic (arithmetic-error) describing a arithmetic error.
func NewArithmeticError(operation Object, operands List, format string, args ...any) *ArithmeticPanic {
	var cond ArithmeticPanic
	cond.hierarchy = arithmeticErrorHierarchy
	cond.Message = fmt.Sprintf(format, args...)
	cond.operation = operation
	cond.operands = operands
	return &cond
}

// PanicArithmetic raises a ArithmeticPanic (arithmetic-error) describing a arithmetic
// error.
func PanicArithmetic(operation Object, operands List, format string, args ...any) {
	panic(NewArithmeticError(operation, operands, format, args...))
}

func makeArithmeticError(args List) Condition {
	var (
		operation Object
		operands  List
		msg       String
	)
	for k, v := range ParseInitList(args) {
		switch k {
		case ":message":
			msg, _ = v.(String)
		case ":operation":
			operation = v
		case ":operands":
			operands, _ = v.(List)
		}
	}
	return NewArithmeticError(operation, operands, "%s", string(msg))
}
