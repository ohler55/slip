// Copyright (c) 2025, Peter Ohler, All rights reserved.

package slip

// DefLister is an interface for objects that implement the DefList()
// function.
type DefLister interface {
	// DefList should return a list that can be evaluated to create the object
	// or nil if that is not possible.
	DefList() List
}
