// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

// SpecialSyntax identifies functions with special syntax for writing and
// reading. This include quote, backquote, and comma.
type SpecialSyntax interface {
	// SpecialChar returns the special character for the function.
	SpecialChar() byte

	// GetArgs returns the function arguments.
	GetArgs() List
}
