// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

type Class interface {
	Object

	// Name of the class.
	Name() string

	// Documentation of the class.
	Documentation() string

	// Describe the class in detail.
	Describe(b []byte, indent, right int, ansi bool) []byte

	// NoMake returns true if the class does not allows creating a new
	// instance with make-instance which should signal an error.
	NoMake() bool

	// MakeInstance creates a new instance but does not call the :init method.
	MakeInstance() Instance
}
