// Copyright (c) 2025, Peter Ohler, All rights reserved.

package clos

import "github.com/ohler55/slip"

type isStandardClass interface {
	slip.Class

	mergeSupers() bool
	slotDefMap() map[string]*SlotDef
	initArgDef(name string) *SlotDef
	initFormMap() map[string]*SlotDef
	defaultsMap() map[string]slip.Object
	precedenceList() []slip.Symbol
	inheritedClasses() []slip.Class

	Ready() bool
	Vars() map[string]slip.Object

	// SlotValue return the value of an instance variable.
	SlotValue(name slip.Symbol) (slip.Object, bool)

	// SetSlotValue sets the value of an instance variable and return true if
	// the name slot exists and was set.
	SetSlotValue(sym slip.Symbol, value slip.Object) (has bool)

	// GetMethod returns the method if it exists.
	GetMethod(name string) *slip.Method

	// MethodNames returns a sorted list of the methods of the class.
	MethodNames() slip.List
}
