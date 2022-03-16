// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

// SequenceSymbol is the symbol with a value of "sequence".
const SequenceSymbol = Symbol("sequence")

func init() {
	DefConstant(SequenceSymbol, SequenceSymbol, `A _sequence_ is an ordered collection of _objects_.`)
}

// Sequence exists to allow assertions to determine if an Object is an sequence.
type Sequence interface {
	Number

	// SequenceType returns the sequence type of the instance which can be one
	// of: list, cons, or vector.
	SequenceType() Symbol
}
