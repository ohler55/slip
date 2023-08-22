// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

import (
	"fmt"
)

// MethodErrorSymbol is the symbol with a value of "invalid-method-error".
const MethodErrorSymbol = Symbol("method-error")

var invalidMethodErrorHierarchy = []Symbol{
	MethodErrorSymbol,
	CellErrorSymbol,
	ErrorSymbol,
	SeriousConditionSymbol,
	ConditionSymbol,
	TrueSymbol,
}

func init() {
	RegisterCondition("method-error", makeMethodError)
}

// MethodError is the interface for all invalid-method-errors.
type MethodError interface {
	CellError

	// IsMethodError need not do anything other than exist.
	IsMethodError()

	// Class returns the class or flavor associated with the method-error.
	Class() Object

	// Qualifier returns the qualifier if any that is associated with the
	// method-error.
	Qualifier() Object
}

// MethodPanic represents a invalid-method-error.
type MethodPanic struct {
	CellPanic
	class     Object
	qualifier Object
}

// IsMethodError need not do anything other than exist.
func (uv *MethodPanic) IsMethodError() {
}

// Class returns the class or flavor associated with the method-error.
func (uv *MethodPanic) Class() Object {
	return uv.class
}

// Qualifier returns the qualifier if any that is associated with the
// method-error.
func (uv *MethodPanic) Qualifier() Object {
	return uv.qualifier
}

// Equal returns true if this Object and the other are equal in value.
func (uv *MethodPanic) Equal(other Object) bool {
	return uv == other
}

// Eval the object.
func (uv *MethodPanic) Eval(s *Scope, depth int) Object {
	return uv
}

// NewMethodError raises a MethodPanic (invalid-method-error)
// describing a invalid-method-error error.
func NewMethodError(class, qualifier, name Object, format string, args ...any) *MethodPanic {
	var cond MethodPanic
	cond.hierarchy = invalidMethodErrorHierarchy
	cond.class = class
	cond.name = name
	cond.qualifier = qualifier
	cond.Message = fmt.Sprintf(format, args...)
	return &cond
}

// PanicMethodError raises a MethodPanic (invalid-method-error)
// describing a invalid-method-error error.
func PanicMethod(class, qualifier, name Object, format string, args ...any) {
	panic(NewMethodError(class, qualifier, name, format, args...))
}

func makeMethodError(args List) Condition {
	var (
		name      Object
		class     Object
		qualifier Object
		msg       String
	)
	for k, v := range ParseInitList(args) {
		switch k {
		case ":name", ":method":
			name = v
		case ":class":
			class = v
		case ":qualifier":
			qualifier = v
		case ":message":
			msg, _ = v.(String)
		}
	}
	if len(msg) == 0 {
		return NewMethodError(class, qualifier, name, "%s %s is not a valid method combination for %s.",
			qualifier, name, class)
	}
	return NewMethodError(class, qualifier, name, "%s", string(msg))
}
