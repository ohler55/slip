// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

// Partial read panic.
type Partial struct {
	// Reason for the panic.
	Reason string
	// Depth of the read when EOF encountered.
	Depth int
}

// String representation of the instance.
func (p *Partial) String() string {
	return p.Reason
}

// Error returns a string representation of the instance.
func (p *Partial) Error() string {
	return p.String()
}
