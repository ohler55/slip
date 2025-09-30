// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import "github.com/ohler55/ojg"

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
	return printer.Append(b, obj, 0)
}

// Readably appends the object to a byte slice. If p.Readbly is true the
// objects is appended in a readable format otherwise a simple append which
// may or may not be readable.
func (obj String) Readably(b []byte, p *Printer) []byte {
	if p.Readably {
		b = ojg.AppendJSONString(b, string(obj), false)
	} else {
		b = append(b, '"')
		b = append(b, obj...)
		b = append(b, '"')
	}
	return b
}

// Simplify the Object into a string.
func (obj String) Simplify() any {
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

// SequenceType returns 'vector.
func (obj String) SequenceType() Symbol {
	return StringSymbol
}

// Length returns the length of the object.
func (obj String) Length() int {
	return len([]rune(obj))
}

// Eval returns self.
func (obj String) Eval(s *Scope, depth int) Object {
	return obj
}

// LoadForm returns a form that can be evaluated to create the object.
func (obj String) LoadForm() Object {
	return obj
}
