// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

import "fmt"

// DivisionByZeroSymbol is the symbol with a value of "division-by-zero".
const DivisionByZeroSymbol = Symbol("division-by-zero")

var divisionByZeroHierarchy = []Symbol{
	DivisionByZeroSymbol,
	ArithmeticErrorSymbol,
	ErrorSymbol,
	SeriousConditionSymbol,
	ConditionSymbol,
	TrueSymbol,
}

func init() {
	RegisterCondition("division-by-zero", makeDivisionByZero)
}

// DivisionByZero is the interface for all arithmetic-errors.
type DivisionByZero interface {
	ArithmeticError

	// IsDivisionByZero need not do anything other than exist.
	IsDivisionByZero()
}

// DivisionByZeroPanic represents a arithmetic-error.
type DivisionByZeroPanic struct {
	ArithmeticPanic
}

// IsDivisionByZero need not do anything other than exist.
func (ap *DivisionByZeroPanic) IsDivisionByZero() {
}

// Equal returns true if this Object and the other are equal in value.
func (ap *DivisionByZeroPanic) Equal(other Object) bool {
	return ap == other
}

// Eval the object.
func (ap *DivisionByZeroPanic) Eval(s *Scope, depth int) Object {
	return ap
}

// NewDivisionByZero creates a DivisionByZeroPanic (arithmetic-error) describing a arithmetic error.
func NewDivisionByZero(operation Object, operands List, format string, args ...any) *DivisionByZeroPanic {
	var cond DivisionByZeroPanic
	cond.hierarchy = divisionByZeroHierarchy
	cond.Message = fmt.Sprintf(format, args...)
	cond.operation = operation
	cond.operands = operands
	return &cond
}

// PanicDivisionByZero raises a DivisionByZeroPanic (division-by-zero)
// describing a division by zero error.
func PanicDivisionByZero(operation Object, operands List, format string, args ...any) {
	panic(NewDivisionByZero(operation, operands, format, args...))
}

func makeDivisionByZero(args List) Condition {
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
	return NewDivisionByZero(operation, operands, "%s", string(msg))
}
