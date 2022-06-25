// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

type novalue byte

// Novalue represents no value. It is used by built in functions such
// apropos that have no return value.
const Novalue = novalue(0)

// String representation of the Object.
func (obj novalue) String() string {
	return string(obj.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (obj novalue) Append(b []byte) []byte {
	return b
}

// Simplify the Object into true.
func (obj novalue) Simplify() interface{} {
	return nil
}

// Equal returns true if this Object and the other are equal in value.
func (obj novalue) Equal(other Object) bool {
	return obj == other
}

// Hierarchy returns the class hierarchy as symbols for the instance.
func (obj novalue) Hierarchy() []Symbol {
	return []Symbol{TrueSymbol}
}

// Eval returns self.
func (obj novalue) Eval(s *Scope, depth int) Object {
	return nil
}
