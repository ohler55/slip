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
}
