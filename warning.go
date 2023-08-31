// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

import "fmt"

// WarningSymbol is the symbol with a value of "serious-condition".
const WarningSymbol = Symbol("warning")

var warningHierarchy = []Symbol{WarningSymbol, ConditionSymbol, TrueSymbol}

func init() {
	RegisterCondition("warning", makeWarning)
}

// Warning is the interface for all warnings. It has no functions that provide
// useful information other than to indicate the type is a Warning which is
// also an Object.
type Warning interface {
	Condition

	// IsWarning need not do anything other than exist.
	IsWarning()

	// Message of the warning.
	Message() string
}

// WarningObj is used to gather a stack trace when panic occurs.
type WarningObj struct {
	ConditionObj

	message string
}

// IsCondition indicates WarningObj is a Condition.
func (w *WarningObj) IsCondition() {
}

// IsWarning indicates WarningObj is a Condition.
func (w *WarningObj) IsWarning() {
}

// Equal returns true if this Object and the other are equal in value.
func (w *WarningObj) Equal(other Object) bool {
	return w == other
}

// Eval the object.
func (w *WarningObj) Eval(s *Scope, depth int) Object {
	return w
}

// Message returns the message.
func (w *WarningObj) Message() string {
	return w.message
}

// Error returns the message.
func (w *WarningObj) Error() string {
	return w.message
}

// NewWarning returns a Warning object.
func NewWarning(format string, args ...any) *WarningObj {
	var w WarningObj
	w.hierarchy = warningHierarchy
	w.message = fmt.Sprintf(format, args...)
	return &w
}

func makeWarning(args List) Condition {
	var msg String
	for k, v := range ParseInitList(args) {
		if k == ":message" {
			msg, _ = v.(String)
		}
	}
	return NewWarning("%s", string(msg))
}
