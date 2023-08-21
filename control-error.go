// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

import "fmt"

// ControlErrorSymbol is the symbol with a value of "control-error".
const ControlErrorSymbol = Symbol("control-error")

var controlErrorHierarchy = []Symbol{
	ControlErrorSymbol,
	ErrorSymbol,
	SeriousConditionSymbol,
	ConditionSymbol,
	TrueSymbol,
}

func init() {
	RegisterCondition("control-error", makeControlError)
}

// ControlError is the interface for all control-errors.
type ControlError interface {
	Error

	// IsControlError need not do anything other than exist.
	IsControlError()
}

// ControlPanic represents a control-error.
type ControlPanic struct {
	Panic
}

// IsControlError need not do anything other than exist.
func (cp *ControlPanic) IsControlError() {
}

// Equal returns true if this Object and the other are equal in value.
func (cp *ControlPanic) Equal(other Object) bool {
	return cp == other
}

// Eval the object.
func (cp *ControlPanic) Eval(s *Scope, depth int) Object {
	return cp
}

// NewControlError creates a ControlPanic (control-error) describing a control error.
func NewControlError(format string, args ...any) *ControlPanic {
	var cond ControlPanic
	cond.hierarchy = controlErrorHierarchy
	cond.Message = fmt.Sprintf(format, args...)
	return &cond
}

// PanicControl raises a ControlPanic (control-error) describing a control
// error.
func PanicControl(format string, args ...any) {
	panic(NewControlError(format, args...))
}

func makeControlError(args List) Condition {
	var msg String

	for k, v := range parseInitList(args) {
		if k == ":message" {
			msg, _ = v.(String)
		}
	}
	return NewControlError("%s", string(msg))
}
