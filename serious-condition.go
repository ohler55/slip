// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

import (
	"strconv"
	"unsafe"
)

// SeriousConditionSymbol is the symbol with a value of "serious-condition".
const SeriousConditionSymbol = Symbol("serious-condition")

func init() {
	RegisterCondition("serious-condition", makeSeriousCondition)
}

// SeriousCondition is the interface for all serious-conditions including
// errors and warnings. It has no functions that provide useful information
// other than to indicate the type is a SeriousCondition which is also an
// Object.
type SeriousCondition interface {
	Condition

	// IsSeriousCondition need not do anything other than exist.
	IsSeriousCondition()
}

// SeriousConditionObj is used to gather a stack trace when panic occurs.
type SeriousConditionObj struct {
	ConditionObj
}

// IsCondition indicates SeriousConditionObj is a Condition.
func (c *SeriousConditionObj) IsCondition() {
}

// Append the object to a byte slice.
func (c *SeriousConditionObj) Append(b []byte) []byte {
	b = append(b, "#<SERIOUS-CONDITION "...)
	b = strconv.AppendUint(b, uint64(uintptr(unsafe.Pointer(c))), 16)
	return append(b, '>')
}

// Simplify the Object into simple go types of nil, bool, int64, float64,
// string, []any, map[string]any, or time.Time.
func (c *SeriousConditionObj) Simplify() any {
	return string(c.Append(nil))
}

// Equal returns true if this Object and the other are equal in value.
func (c *SeriousConditionObj) Equal(other Object) bool {
	return c == other
}

// Hierarchy returns the class hierarchy as symbols for the instance.
func (c *SeriousConditionObj) Hierarchy() []Symbol {
	return []Symbol{SeriousConditionSymbol, ConditionSymbol, ConditionSymbol, TrueSymbol}
}

// Eval the object.
func (c *SeriousConditionObj) Eval(s *Scope, depth int) Object {
	return c
}

// Error returns the panic message.
func (c *SeriousConditionObj) Error() string {
	return string(c.Append(nil))
}

// String returns the panic message.
func (c *SeriousConditionObj) String() string {
	return string(c.Append(nil))
}

func makeSeriousCondition(args List) Condition {
	return &SeriousConditionObj{}
}
