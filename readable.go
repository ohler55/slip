// Copyright (c) 2025, Peter Ohler, All rights reserved.

package slip

// Readble identifies objects that can be written readably.
type Readble interface {
	// Readably appends the object to a byte slice. If p.Readbly is true the
	// objects is appended in a readable format otherwise a simple append
	// which may or may not be readable.
	Readably(b []byte, p *Printer) []byte
}
