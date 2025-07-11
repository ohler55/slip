// Copyright (c) 2025, Peter Ohler, All rights reserved.

package clos

import "github.com/ohler55/slip"

type isStandardClass interface {
	slip.Class
	standardClass() *StandardClass

	Ready() bool
	mergeSupers() bool

	// SlotValue return the value of an instance variable.
	SlotValue(name slip.Symbol) (slip.Object, bool)

	// SetSlotValue sets the value of an instance variable and return true if
	// the name slot exists and was set.
	SetSlotValue(sym slip.Symbol, value slip.Object) (has bool)
}
