// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

// Funky is an interface shared by all functions.
type Funky interface {
	// GetArgs returns the function arguments.
	GetArgs() []Object

	// GetName returns the function name.
	GetName() string

	// Apply evaluates with the need to evaluate the args.
	Apply(args List, depth int) Object
}
