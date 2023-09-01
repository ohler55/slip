// Copyright (c) 2023, Peter Ohler, All rights reserved.

package class

import "github.com/ohler55/slip"

type Classy interface {

	// Name of the class.
	Name() string

	// Slots return the slots available to instances of the class.
	Slots() []slip.Symbol

	// Supers returns all the superclasses of the class or just the direct ones.
	Supers(direct bool) []slip.Symbol

	// Subs returns all the subclasses of the class or just the direct ones.
	Subs(direct bool) []slip.Symbol

	// Methods of the class either just the direct methods of all inherited
	// methods.
	Methods(direct bool) []slip.Symbol

	// Documentation of the class.
	Documentation() string

	// Final returns true if the class can not be modified.
	Final() bool

	// Hierarchy returns the class hierarchy as symbols for the instance.
	Hierarchy() []slip.Symbol

	// Prototype or sample value.
	Prototype() slip.Object
}
