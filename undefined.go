// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

type undefined int

const undef = undefined(0)

// String representation of the Object.
func (obj undefined) String() string {
	return string(obj.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (obj undefined) Append(b []byte) []byte {
	return append(b, "#<unbound>"...)
}

// Simplify the Object into nil.
func (obj undefined) Simplify() interface{} {
	return nil
}

// Equal returns true if this Object and the other are equal in value.
func (obj undefined) Equal(other Object) bool {
	return obj == other
}

// Hierarchy returns the class hierarchy as symbols for the instance.
func (obj undefined) Hierarchy() []Symbol {
	return []Symbol{Symbol("undefined"), TrueSymbol}
}

// Eval returns self.
func (obj undefined) Eval(s *Scope, depth int) Object {
	return obj
}
