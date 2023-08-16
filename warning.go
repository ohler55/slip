// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

// WarningSymbol is the symbol with a value of "serious-condition".
const WarningSymbol = Symbol("warning")

// Warning is the interface for all warnings. It has no functions that provide
// useful information other than to indicate the type is a Warning which is
// also an Object.
type Warning interface {
	Condition

	// IsWarning need not do anything other than exist.
	IsWarning()
}

// WarningObj is used to gather a stack trace when panic occurs.
type WarningObj struct {
	Message string
}

// IsCondition indicates WarningObj is a Condition.
func (w *WarningObj) IsCondition() {
}

// IsWarning indicates WarningObj is a Condition.
func (w *WarningObj) IsWarning() {
}

// Append the object to a byte slice.
func (w *WarningObj) Append(b []byte) []byte {
	return append(b, w.Message...)
}

// Simplify the Object into simple go types of nil, bool, int64, float64,
// string, []any, map[string]any, or time.Time.
func (w *WarningObj) Simplify() any {
	return w.Message
}

// Equal returns true if this Object and the other are equal in value.
func (w *WarningObj) Equal(other Object) bool {
	return w == other
}

// Hierarchy returns the class hierarchy as symbols for the instance.
func (w *WarningObj) Hierarchy() []Symbol {
	return []Symbol{WarningSymbol, ConditionSymbol, TrueSymbol}
}

// Eval the object.
func (w *WarningObj) Eval(s *Scope, depth int) Object {
	return w
}

// Error returns the panic message.
func (w *WarningObj) Error() string {
	return w.Message
}

// String returns the panic message.
func (w *WarningObj) String() string {
	return w.Message
}
