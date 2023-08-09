// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

// PartialPanic read panic.
type PartialPanic struct {
	ParsePanic

	// Depth of the read when EOF encountered.
	Depth int
}
