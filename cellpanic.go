// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

import "fmt"

// CellPanic represents a cell-error.
type CellPanic struct {
	Panic
	name string
}

// IsCellError need not do anything other than exist.
func (cp *CellPanic) IsCellError() {
}

// Hierarchy returns the class hierarchy as symbols for the instance.
func (cp *CellPanic) Hierarchy() []Symbol {
	return []Symbol{CellErrorSymbol, ErrorSymbol, SeriousConditionSymbol, ConditionSymbol, TrueSymbol}
}

// Eval the object.
func (cp *CellPanic) Eval(s *Scope, depth int) Object {
	return cp
}

// Name returns the name associated with the error.
func (cp *CellPanic) Name() string {
	return cp.name
}

// PanicCell raises a CellPanic (cell-error) describing a cell
// error.
func PanicCell(name string, format string, args ...any) {
	panic(&CellPanic{
		Panic: Panic{Message: fmt.Sprintf(format, args...)},
		name:  name,
	})
}
