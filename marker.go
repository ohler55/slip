// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

type marker byte

// String representation of the Object.
func (obj marker) String() string {
	return string([]byte{byte(obj)})
}

// Append a buffer with a representation of the Object.
func (obj marker) Append(b []byte) []byte {
	return append(b, byte(obj))
}

// Simplify the Object into true.
func (obj marker) Simplify() interface{} {
	return string([]byte{byte(obj)})
}

// Equal returns true if this Object and the other are equal in value.
func (obj marker) Equal(other Object) bool {
	return obj == other
}

// Hierarchy returns the class hierarchy as symbols for the instance.
func (obj marker) Hierarchy() []Symbol {
	return []Symbol{TrueSymbol}
}

// Eval returns self.
func (obj marker) Eval(s *Scope, depth int) Object {
	return obj
}
