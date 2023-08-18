// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

import "fmt"

// UndefinedFunctionSymbol is the symbol with a value of "undefined-function".
const UndefinedFunctionSymbol = Symbol("undefined-function")

var undefinedFunctionHierarchy = []Symbol{
	UndefinedFunctionSymbol,
	CellErrorSymbol,
	ErrorSymbol,
	SeriousConditionSymbol,
	ConditionSymbol,
	TrueSymbol,
}

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

// Equal returns true if this Object and the other are equal in value.
func (uf *UndefinedFunctionPanic) Equal(other Object) bool {
	return uf == other
}

// Eval the object.
func (uf *UndefinedFunctionPanic) Eval(s *Scope, depth int) Object {
	return uf
}

// NewUndefinedFunction returns a new UndefinedFunctionPanic
// (undefined-function) describing a undefined-function error.
func NewUndefinedFunction(name Object, format string, args ...any) *UndefinedFunctionPanic {
	var cond UndefinedFunctionPanic
	cond.hierarchy = undefinedFunctionHierarchy
	cond.name = name
	cond.Message = fmt.Sprintf(format, args...)
	return &cond
}

// PanicUndefinedFunction raises a UndefinedFunctionPanic (undefined-function)
// describing a undefined-function error.
func PanicUndefinedFunction(name Object, format string, args ...any) {
	panic(NewUndefinedFunction(name, format, args...))
}

func makeUndefinedFunction(args List) Condition {
	var (
		name Object
		msg  String
	)
	for k, v := range parseInitList(args) {
		switch k {
		case ":name":
			name = v
		case ":message":
			msg, _ = v.(String)
		}
	}
	return NewUndefinedFunction(name, "%s", string(msg))
}
