// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

type evalue struct {
	value Object
}

// String representation of the Object.
func (ev evalue) String() string {
	return ObjectString(ev.value)
}

// Append a buffer with a representation of the Object.
func (ev evalue) Append(b []byte) []byte {
	return ObjectAppend(b, ev.value)
}

// Simplify the Object into true.
func (ev evalue) Simplify() any {
	return SimpleObject(ev.value)
}

// Equal returns true if this Object and the other are equal in value.
func (ev evalue) Equal(other Object) bool {
	return ev == other
}

// Hierarchy returns the class hierarchy as symbols for the instance.
func (ev evalue) Hierarchy() []Symbol {
	return []Symbol{TrueSymbol}
}

// Eval returns self.
func (ev evalue) Eval(s *Scope, depth int) Object {
	return ev.value
}
