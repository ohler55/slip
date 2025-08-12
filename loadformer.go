// Copyright (c) 2025, Peter Ohler, All rights reserved.

package slip

// LoadFormer is an interface for objects that implement the LoadForm()
// function.
type LoadFormer interface {
	// LoadForm should return a form that can be evaluated to create the object
	// or nil if that is not possible.
	LoadForm() Object
}
