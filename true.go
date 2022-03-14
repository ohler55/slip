// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

// TrueSymbol is the symbol with a value of "t".
const TrueSymbol = Symbol("t")

// Only has one object/value which is True
type boolean bool

// True is the true boolean value.
const True = boolean(true)

// String representation of the Object.
func (obj boolean) String() string {
	return "t"
}

// Append a buffer with a representation of the Object.
func (obj boolean) Append(b []byte) []byte {
	return append(b, 't')
}

// Simplify the Object into true.
func (obj boolean) Simplify() interface{} {
	return true
}

// Equal returns true if this Object and the other are equal in value.
func (obj boolean) Equal(other Object) bool {
	return obj == other
}

// Hierarchy returns the class hierarchy as symbols for the instance.
func (obj boolean) Hierarchy() []Symbol {
	return []Symbol{TrueSymbol}
}
