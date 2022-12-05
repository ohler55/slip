// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

// StringSymbol is the symbol with a value of "string".
const StringSymbol = Symbol("string")

func init() {
	DefConstant(StringSymbol, StringSymbol, `A _string_ is linear collection of _characters_.`)
}

// String is a string Object.
type String string

// String representation of the Object.
func (obj String) String() string {
	return string(obj.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (obj String) Append(b []byte) []byte {
	return printer.Append(b, obj, 0)
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

// Eval returns self.
func (obj String) Eval(s *Scope, depth int) Object {
	return obj
}
