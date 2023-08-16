// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

import "fmt"

// CellErrorSymbol is the symbol with a value of "cell-error".
const CellErrorSymbol = Symbol("cell-error")

func init() {
	RegisterCondition("cell-error", makeCellError)
}

// CellError is the interface for all cell-errors.
type CellError interface {
	Error

	// IsCellError need not do anything other than exist.
	IsCellError()

	// Name returns the name associated with the error.
	Name() Object
}

// CellPanic represents a cell-error.
type CellPanic struct {
	Panic
	name Object
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
func (cp *CellPanic) Name() Object {
	return cp.name
}

// PanicCell raises a CellPanic (cell-error) describing a cell
// error.
func PanicCell(name Object, format string, args ...any) {
	panic(&CellPanic{
		Panic: Panic{Message: fmt.Sprintf(format, args...)},
		name:  name,
	})
}

func makeCellError(args List) Condition {
	c := &CellPanic{}
	for k, v := range parseInitList(args) {
		if k == ":name" {
			c.name = v
		}
	}
	return c
}
