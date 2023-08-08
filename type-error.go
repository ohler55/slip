// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

// TypeErrorSymbol is the symbol with a value of "type-error".
const TypeErrorSymbol = Symbol("type-error")

// TypeError is the interface for all type-errors.
type TypeError interface {
	Error

	// IsTypeError need not do anything other than exist.
	IsTypeError()

	// Datum is the object provided.
	Datum() Object

	// ExpectedTypes are the expected types. This deviates from common LSIP
	// where this is only one expected-type.
	ExpectedTypes() Object
}
