// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

// BoundCaller is an interface shared functions that can be invoked with
// arguments already bound to the scope.
type BoundCaller interface {
	// BoundCall applies a function to a set of already evaluated and bound
	// arguments.
	BoundCall(s *Scope, depth int) Object
}
