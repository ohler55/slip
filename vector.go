// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

// VectorSymbol is the symbol with a value of "vector".
const VectorSymbol = Symbol("vector")

func init() {
	DefConstant(VectorSymbol, VectorSymbol, `A _vector_ one dimensional array of _objects_.`)
}

// Vector is a vector Object.
type Vector []Object

// String representation of the Object.
func (obj Vector) String() string {
	return string(obj.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (obj Vector) Append(b []byte) []byte {
	b = append(b, "#("...)
	for i, v := range obj {
		if 0 < i {
			b = append(b, ' ')
		}
		if v == nil {
			b = append(b, "nil"...)
		} else {
			b = v.Append(b)
		}
	}
	return append(b, ')')
}

// Simplify the Object into a []interface{}.
func (obj Vector) Simplify() interface{} {
	out := make([]interface{}, len(obj))
	for i, o := range obj {
		if o == nil {
			out[i] = nil
		} else {
			out[i] = o.Simplify()
		}
	}
	return out
}

// Equal returns true if this Object and the other are equal in value.
func (obj Vector) Equal(other Object) (eq bool) {
	if to, ok := other.(Vector); ok {
		if len(obj) == len(to) {
			eq = true
			for i, co := range obj {
				if !ObjectEqual(co, to[i]) {
					eq = false
					break
				}
			}
		}
	}
	return
}

// Hierarchy returns the class hierarchy as symbols for the instance.
func (obj Vector) Hierarchy() []Symbol {
	return []Symbol{VectorSymbol, ArraySymbol, SequenceSymbol, TrueSymbol}
}

// SequenceType returns 'vector.
func (obj Vector) SequenceType() Symbol {
	return VectorSymbol
}

// Length returns the length of the object.
func (obj Vector) Length() int {
	return len(obj)
}

// ArrayType returns 'vector.
func (obj Vector) ArrayType() Symbol {
	return VectorSymbol
}

// Eval returns self.
func (obj Vector) Eval(s *Scope, depth int) Object {
	return obj
}
