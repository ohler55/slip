// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

// Describer is the interface for types that respond to Describe() for use in
// the cl::describe function.
type Describer interface {
	// Describe the instance in detail.
	Describe(b []byte, indent, right int, ansi bool) []byte
}
