// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

// Caller is an interface shared by all functions.
type Caller interface {
	// Call applies a function to a set of already evaluated arguments.
	Call(s *Scope, args List, depth int) Object
}
