// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

import "fmt"

// ClassNotFoundSymbol is the symbol with a value of "unbound-slot".
const ClassNotFoundSymbol = Symbol("class-not-found")

// ClassNotFound is the interface for the class-not-found error.
type ClassNotFound interface {
	CellError

	// IsClassNotFound need not do anything other than exist.
	IsClassNotFound()
}

// ClassNotFoundPanic represents a unbound-slot.
type ClassNotFoundPanic struct {
	CellPanic
	instance Object
}

// IsClassNotFound need not do anything other than exist.
func (uv *ClassNotFoundPanic) IsClassNotFound() {
}

// Hierarchy returns the class hierarchy as symbols for the instance.
func (uv *ClassNotFoundPanic) Hierarchy() []Symbol {
	return []Symbol{
		ClassNotFoundSymbol,
		CellErrorSymbol,
		ErrorSymbol,
		SeriousConditionSymbol,
		ConditionSymbol,
		TrueSymbol,
	}
}

// Eval the object.
func (uv *ClassNotFoundPanic) Eval(s *Scope, depth int) Object {
	return uv
}

// PanicClassNotFound raises a ClassNotFoundPanic (unbound-slot)
// describing a unbound-slot error.
func PanicClassNotFound(name string, format string, args ...any) {
	panic(&ClassNotFoundPanic{
		CellPanic: CellPanic{
			Panic: Panic{Message: fmt.Sprintf(format, args...)},
			name:  name,
		},
	})
}
