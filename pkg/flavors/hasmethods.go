// copyright (c) 2023, Peter Ohler, All rights reserved.

package flavors

// HasMethods is an interface for objects that have documentation.
type HasMethods interface {
	// GetMethod returns the method if it exists.
	GetMethod(name string) []*Method
}
