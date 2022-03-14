// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

// Var is a variable.
type Var struct {
	// Name identified the variable.
	Name Symbol

	// Value is the value of the Variable.
	Value Object

	// Doc is the documentation for the variable.
	Doc string
}
