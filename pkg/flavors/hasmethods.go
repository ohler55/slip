// copyright (c) 2023, Peter Ohler, All rights reserved.

package flavors

import "github.com/ohler55/slip"

// HasMethods is an interface for objects that have documentation.
type HasMethods interface {
	// GetMethod returns the method if it exists.
	GetMethod(name string) *slip.Method

	// Methods returns a map of the methods.
	Methods() map[string]*slip.Method
}
