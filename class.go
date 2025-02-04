// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

import "strings"

var allClasses = map[string]Class{}

type Class interface {
	Object

	// Name of the class.
	Name() string

	// Documentation of the class.
	Documentation() string

	// SetDocumentation of the class.
	SetDocumentation(doc string)

	// Describe the class in detail.
	Describe(b []byte, indent, right int, ansi bool) []byte

	// NoMake returns true if the class does not allows creating a new
	// instance with make-instance which should signal an error.
	NoMake() bool

	// MakeInstance creates a new instance but does not call the :init method.
	MakeInstance() Instance
}

// Find finds the named class.
func FindClass(name string) (c Class) {
	if c = allClasses[name]; c == nil {
		c = allClasses[strings.ToLower(name)]
	}
	return
}

// RegisterClass a class.
func RegisterClass(name string, c Class) {
	name = strings.ToLower(name)
	allClasses[name] = c
}
