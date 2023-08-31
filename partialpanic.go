// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

import "fmt"

// PartialPanic read panic.
type PartialPanic struct {
	ParsePanic

	// Depth of the read when EOF encountered.
	Depth int
}

// NewPartial creates a new PartialPanic.
func NewPartial(depth int, format string, args ...any) *PartialPanic {
	var p PartialPanic
	p.hierarchy = parseErrorHierarchy
	p.Depth = depth
	p.Message = fmt.Sprintf(format, args...)
	return &p
}
