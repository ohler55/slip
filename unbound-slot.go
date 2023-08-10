// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

// UnboundSlotSymbol is the symbol with a value of "unbound-slot".
const UnboundSlotSymbol = Symbol("unbound-slot")

// UnboundSlot is the interface for all unbound-slots.
type UnboundSlot interface {
	CellError

	// IsUnboundSlot need not do anything other than exist.
	IsUnboundSlot()

	// Instance returns the instance associated with the unbound slot.
	Instance() Object
}
