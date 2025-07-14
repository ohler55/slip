// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

import "fmt"

// PartialPanic read panic.
type PartialPanic struct {
	// Depth of the read when EOF encountered.
	Depth int
	// Message for the error.
	Message string
}

// Error
func (pp *PartialPanic) Error() string {
	return pp.Message
}

// NewPartial creates a new PartialPanic.
func NewPartial(depth int, format string, args ...any) *PartialPanic {
	return &PartialPanic{
		Depth:   depth,
		Message: fmt.Sprintf(format, args...),
	}
}
