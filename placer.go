// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

// Placer interface for use with functions or macros that expect a
// placer. (e.g., setf, incf)
type Placer interface {
	Funky
	// Place the value in the location that the function would normally return
	// with an Eval with the same arguments.
	Place(args List, value Object)
}
