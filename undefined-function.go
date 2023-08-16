// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

import "fmt"

// UndefinedFunctionSymbol is the symbol with a value of "undefined-function".
const UndefinedFunctionSymbol = Symbol("undefined-function")

func init() {
	RegisterCondition("undefined-function", makeUndefinedFunction)
}

// UndefinedFunction is the interface for all undefined-functions.
type UndefinedFunction interface {
	CellError

	// IsUndefinedFunction need not do anything other than exist.
	IsUndefinedFunction()
}

// UndefinedFunctionPanic represents a undefinedFunction-error.
type UndefinedFunctionPanic struct {
	CellPanic
}

// IsUndefinedFunctionError need not do anything other than exist.
func (uf *UndefinedFunctionPanic) IsUndefinedFunction() {
}

// Hierarchy returns the class hierarchy as symbols for the instance.
func (uf *UndefinedFunctionPanic) Hierarchy() []Symbol {
	return []Symbol{
		UndefinedFunctionSymbol,
		CellErrorSymbol,
		ErrorSymbol,
		SeriousConditionSymbol,
		ConditionSymbol,
		TrueSymbol,
	}
}

// Eval the object.
func (uf *UndefinedFunctionPanic) Eval(s *Scope, depth int) Object {
	return uf
}

// PanicUndefinedFunction raises a UndefinedFunctionPanic (undefined-function)
// describing a undefined-function error.
func PanicUndefinedFunction(name Object, format string, args ...any) {
	panic(&UndefinedFunctionPanic{
		CellPanic: CellPanic{
			Panic: Panic{Message: fmt.Sprintf(format, args...)},
			name:  name,
		},
	})
}

func makeUndefinedFunction(args List) Condition {
	c := &UndefinedFunctionPanic{}
	for k, v := range parseInitList(args) {
		if k == ":name" {
			c.name = v
		}
	}
	return c
}
