// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

type Instance interface {
	Object

	// Class of the instance.
	Class() Class

	// Init the instance slots from the provided args list. If the scope is
	// not nil then send :init is called.
	Init(scope *Scope, args List, depth int)

	// TBD maybe ChangeClass(nc Classy)

	// Receive a method invocation from the send function. It is typically
	// called by the send function but can be called directly so effectively
	// send a method to an instance.
	Receive(s *Scope, message string, args List, depth int) Object

	// HasMethod returns true if the instance handles the named method.
	HasMethod(method string) bool
}
