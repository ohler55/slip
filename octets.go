// Copyright (c) 2024, Peter Ohler, All rights reserved.

package slip

import "bytes"

// OctetsSymbol is the symbol with a value of "octets".
const OctetsSymbol = Symbol("octets")

func init() {
	DefConstant(OctetsSymbol, OctetsSymbol, `A _octets_ one dimensional array of _octet_.`)
}

// Octets is a octets Object.
type Octets []byte

// NewOctets creates a new Octets. If fillPtr is not used then it should be -1.
func NewOctets(dim int, initElement Octet) Octets {
	octs := make(Octets, dim)
	for i := len(octs) - 1; 0 <= i; i-- {
		octs[i] = byte(initElement)
	}
	return octs
}

// String representation of the Object.
func (obj Octets) String() string {
	return string(obj.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (obj Octets) Append(b []byte) []byte {
	b = append(b, "#("...)
	for i, v := range obj {
		if 0 < i {
			b = append(b, ' ')
		}
		b = printer.Append(b, Octet(v), 0)
	}
	return append(b, ')')
}

// Simplify the Object into a []any.
func (obj Octets) Simplify() any {
	out := make([]any, len(obj))
	for i, v := range obj {
		out[i] = int64(v)
	}
	return out
}

// Equal returns true if this Object and the other are equal in value.
func (obj Octets) Equal(other Object) (eq bool) {
	if to, ok := other.(Octets); ok {
		eq = bytes.Equal([]byte(obj), []byte(to))
	}
	return
}

// Hierarchy returns the class hierarchy as symbols for the instance.
func (obj Octets) Hierarchy() []Symbol {
	return []Symbol{OctetsSymbol, VectorSymbol, ArraySymbol, SequenceSymbol, TrueSymbol}
}

// SequenceType returns 'octets.
func (obj Octets) SequenceType() Symbol {
	return OctetSymbol
}

// Length returns the length of the object.
func (obj Octets) Length() int {
	return len(obj)
}

// ArrayType returns 'octets.
func (obj Octets) ArrayType() Symbol {
	return OctetsSymbol
}

// Eval returns self.
func (obj Octets) Eval(s *Scope, depth int) Object {
	return obj
}

// AsList the Object into a list.
func (obj Octets) AsList() List {
	list := make(List, len(obj))
	for i, v := range obj {
		list[i] = Octet(v)
	}
	return list
}
