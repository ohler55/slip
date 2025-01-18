// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

// Undefined represents an undefined value.
type Undefined string

// String representation of the Object.
func (obj Undefined) String() string {
	return string(obj.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (obj Undefined) Append(b []byte) []byte {
	return append(b, obj...)
}

// Simplify the Object into true.
func (obj Undefined) Simplify() any {
	return nil
}

// Equal returns true if this Object and the other are equal in value.
func (obj Undefined) Equal(other Object) bool {
	return obj == other
}

// Hierarchy returns the class hierarchy as symbols for the instance.
func (obj Undefined) Hierarchy() []Symbol {
	return []Symbol{TrueSymbol}
}

// Eval returns self.
func (obj Undefined) Eval(s *Scope, depth int) Object {
	panic(NewUndefinedFunction(obj, "Function %s is not defined.", obj))
}
