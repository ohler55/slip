// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

import (
	"fmt"
)

// UnboundSlotSymbol is the symbol with a value of "unbound-slot".
const UnboundSlotSymbol = Symbol("unbound-slot")

var unboundSlotHierarchy = []Symbol{
	UnboundSlotSymbol,
	CellErrorSymbol,
	ErrorSymbol,
	SeriousConditionSymbol,
	ConditionSymbol,
	TrueSymbol,
}

func init() {
	RegisterCondition("unbound-slot", makeUnboundSlot)
}

// UnboundSlot is the interface for all unbound-slots.
type UnboundSlot interface {
	CellError

	// IsUnboundSlot need not do anything other than exist.
	IsUnboundSlot()

	// Instance returns the instance associated with the unbound slot.
	Instance() Object
}

// UnboundSlotPanic represents a unbound-slot.
type UnboundSlotPanic struct {
	CellPanic
	instance Object
}

// IsUnboundSlot need not do anything other than exist.
func (uv *UnboundSlotPanic) IsUnboundSlot() {
}

// Instance need not do anything other than exist.
func (uv *UnboundSlotPanic) Instance() Object {
	return uv.instance
}

// Equal returns true if this Object and the other are equal in value.
func (uv *UnboundSlotPanic) Equal(other Object) bool {
	return uv == other
}

// Eval the object.
func (uv *UnboundSlotPanic) Eval(s *Scope, depth int) Object {
	return uv
}

// NewUnboundSlot raises a UnboundSlotPanic (unbound-slot)
// describing a unbound-slot error.
func NewUnboundSlot(instance Object, name Object, format string, args ...any) *UnboundSlotPanic {
	var cond UnboundSlotPanic
	cond.hierarchy = unboundSlotHierarchy
	cond.instance = instance
	cond.name = name
	if 0 < len(format) {
		cond.Message = fmt.Sprintf(format, args...)
	} else {
		cond.Message = fmt.Sprintf("The slot %s is unbound in the object %s.", name, instance)
	}
	return &cond
}

// PanicUnboundSlot raises a UnboundSlotPanic (unbound-slot)
// describing a unbound-slot error.
func PanicUnboundSlot(instance Object, name Object, format string, args ...any) {
	panic(NewUnboundSlot(instance, name, format, args...))
}

func makeUnboundSlot(args List) Condition {
	var (
		name     Object
		instance Object
		msg      String
		format   string
	)

	for k, v := range ParseInitList(args) {
		switch k {
		case ":name":
			name = v
		case ":instance":
			instance = v
		case ":message":
			msg, _ = v.(String)
			format = "%s"
		}
	}
	return NewUnboundSlot(instance, name, format, string(msg))
}
