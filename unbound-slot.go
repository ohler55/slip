// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

import "fmt"

// UnboundSlotSymbol is the symbol with a value of "unbound-slot".
const UnboundSlotSymbol = Symbol("unbound-slot")

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

// Hierarchy returns the class hierarchy as symbols for the instance.
func (uv *UnboundSlotPanic) Hierarchy() []Symbol {
	return []Symbol{
		UnboundSlotSymbol,
		CellErrorSymbol,
		ErrorSymbol,
		SeriousConditionSymbol,
		ConditionSymbol,
		TrueSymbol,
	}
}

// Eval the object.
func (uv *UnboundSlotPanic) Eval(s *Scope, depth int) Object {
	return uv
}

// PanicUnboundSlot raises a UnboundSlotPanic (unbound-slot)
// describing a unbound-slot error.
func PanicUnboundSlot(instance Object, name Object, format string, args ...any) {
	panic(&UnboundSlotPanic{
		CellPanic: CellPanic{
			Panic: Panic{Message: fmt.Sprintf(format, args...)},
			name:  name,
		},
		instance: instance,
	})
}

func makeUnboundSlot(args List) Condition {
	c := &UnboundSlotPanic{}
	for k, v := range parseInitList(args) {
		switch k {
		case ":name":
			c.name = v
		case ":instance":
			c.instance = v
		}
	}
	return c
}
