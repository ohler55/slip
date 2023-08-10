// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

import "fmt"

// UndefinedFunctionPanic represents a undefinedFunction-error.
type UndefinedFunctionPanic struct {
	CellPanic
}

// IsUndefinedFunctionError need not do anything other than exist.
func (uf *UndefinedFunctionPanic) IsUndefinedFunctionError() {
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
func PanicUndefinedFunction(name string, format string, args ...any) {
	panic(&UndefinedFunctionPanic{
		CellPanic: CellPanic{
			Panic: Panic{Message: fmt.Sprintf(format, args...)},
			name:  name,
		},
	})
}
