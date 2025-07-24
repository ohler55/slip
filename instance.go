// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

type Instance interface {
	Object
	Receiver

	// Class of the instance.
	Class() Class

	// IsA returns true if the instance's class is the specified class or a
	// sub-class of the specified class.
	IsA(class string) bool

	// Init the instance slots from the provided args list. If the scope is
	// not nil then send :init is called.
	Init(scope *Scope, args List, depth int)

	// SetSynchronized sets the instance to be thread safe (mutex protected
	// slots) or not.
	SetSynchronized(on bool)

	// Synchronized returns true if the instance is thread safe.
	Synchronized() bool

	// SlotNames returns a list of the slots names for the instance.
	SlotNames() []string

	// SlotValue return the value of an instance variable.
	SlotValue(name Symbol) (Object, bool)

	// SetSlotValue sets the value of an instance variable and return true if
	// the name slot exists and was set.
	SetSlotValue(sym Symbol, value Object) (has bool)

	// GetMethod returns the method if it exists.
	GetMethod(name string) *Method

	// MethodNames returns a sorted list of the methods of the class.
	MethodNames() List

	// ID returns unique ID for the instance.
	ID() uint64

	// Dup returns a duplicate of the instance.
	Dup() Instance
}
