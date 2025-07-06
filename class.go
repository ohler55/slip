// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

type Class interface {
	Object

	// Name of the class.
	Name() string

	// Package the class is defined in.
	Pkg() *Package

	// Documentation of the class.
	Documentation() string

	// SetDocumentation of the class.
	SetDocumentation(doc string)

	// Describe the class in detail.
	Describe(b []byte, indent, right int, ansi bool) []byte

	// MakeInstance creates a new instance but does not call the :init method.
	MakeInstance() Instance

	// Inherits returns true if this Class inherits from a specified Class.
	Inherits(c Class) bool

	// DefList should return a list that can be evaluated to create the class
	// or nil if the class is a built in class. As an example, a flavor would
	// be created by a defflavor expression.
	DefList() List
}

// Find finds the named class.
func FindClass(name string) (c Class) {
	return CurrentPackage.FindClass(name)
}

// RegisterClass a class.
func RegisterClass(name string, c Class) {
	p := c.Pkg()
	if p == nil {
		p = CurrentPackage
	}
	p.RegisterClass(name, c)
}
