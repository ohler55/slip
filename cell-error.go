// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

import (
	"fmt"
)

// CellErrorSymbol is the symbol with a value of "cell-error".
const CellErrorSymbol = Symbol("cell-error")

var cellErrorHierarchy = []Symbol{CellErrorSymbol, ErrorSymbol, SeriousConditionSymbol, ConditionSymbol, TrueSymbol}

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

// Equal returns true if this Object and the other are equal in value.
func (cp *CellPanic) Equal(other Object) bool {
	return cp == other
}

// Eval the object.
func (cp *CellPanic) Eval(s *Scope, depth int) Object {
	return cp
}

// Name returns the name associated with the error.
func (cp *CellPanic) Name() Object {
	return cp.name
}

// NewCellError raises a CellPanic (cell-error) describing a cell
// error.
func NewCellError(name Object, format string, args ...any) *CellPanic {
	var cond CellPanic
	cond.hierarchy = unboundSlotHierarchy
	cond.name = name
	cond.Message = fmt.Sprintf(format, args...)
	return &cond
}

// PanicCell raises a CellPanic (cell-error) describing a cell
// error.
func PanicCell(name Object, format string, args ...any) {
	panic(NewCellError(name, format, args...))
}

func makeCellError(args List) Condition {
	var c CellPanic
	c.hierarchy = cellErrorHierarchy
	for k, v := range parseInitList(args) {
		switch k {
		case ":name":
			c.name = v
		case ":message":
			msg, _ := v.(String)
			c.Message = string(msg)
		}
	}
	return &c
}
