// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

// StringSymbol is the symbol with a value of "string".
const StringSymbol = Symbol("string")

// String is a string Object.
type String string

// String representation of the Object.
func (obj String) String() string {
	return string(obj.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (obj String) Append(b []byte) []byte {
	b = append(b, '"')
	b = append(b, obj...)
	return append(b, '"')
}

// Simplify the Object into a string.
func (obj String) Simplify() interface{} {
	return string(obj)
}

// Equal returns true if this Object and the other are equal in value.
func (obj String) Equal(other Object) bool {
	return obj == other
}

// Hierarchy returns the class hierarchy as symbols for the instance.
func (obj String) Hierarchy() []Symbol {
	return []Symbol{StringSymbol, VectorSymbol, ArraySymbol, SequenceSymbol, TrueSymbol}
}
