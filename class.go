// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

type Class interface {

	// Name of the class.
	Name() string

	// Documentation of the class.
	Documentation() string

	// Describe the class in detail.
	Describe(b []byte, indent, right int, ansi bool) []byte

	// Abstract returns true if the class is an abstract flavor or if
	// make-instance should signal an error..
	Abstract() bool
}
