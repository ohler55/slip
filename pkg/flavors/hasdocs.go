// Copyright (c) 2022, Peter Ohler, All rights reserved.

package flavors

// HasDocs is an interface for objects that have documentation.
type HasDocs interface {
	// Docs returns the documentation for the object.
	Docs() string
}
