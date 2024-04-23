// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

// VectorSymbol is the symbol with a value of "vector".
const VectorSymbol = Symbol("vector")

func init() {
	DefConstant(VectorSymbol, VectorSymbol, `A _vector_ one dimensional array of _objects_.`)
}

// Vector is a vector Object.
type Vector struct {
	Array
	FillPtr int
}

// NewVector creates a new Vector. If fillPtr is not used then it should be -1.
func NewVector(elements List, elementType Symbol, adjustable bool) *Vector {
	return &Vector{
		Array: Array{
			dims:        []int{len(elements)},
			sizes:       []int{1},
			elements:    elements,
			elementType: elementType,
			adjustable:  adjustable,
		},
		FillPtr: -1,
	}
}

// String representation of the Object.
func (obj *Vector) String() string {
	return string(obj.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (obj *Vector) Append(b []byte) []byte {
	b = append(b, "#("...)
	for i, v := range obj.elements {
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
func (obj *Vector) Simplify() interface{} {
	out := make([]interface{}, len(obj.elements))
	for i, o := range obj.elements {
		if o == nil {
			out[i] = nil
		} else {
			out[i] = o.Simplify()
		}
	}
	return out
}

// Equal returns true if this Object and the other are equal in value.
func (obj *Vector) Equal(other Object) (eq bool) {
	if to, ok := other.(*Vector); ok {
		if len(obj.elements) == len(to.elements) &&
			to.elementType == obj.elementType &&
			to.adjustable == obj.adjustable {
			eq = true
			for i, co := range obj.elements {
				if !ObjectEqual(co, to.elements[i]) {
					eq = false
					break
				}
			}
		}
	}
	return
}

// Hierarchy returns the class hierarchy as symbols for the instance.
func (obj *Vector) Hierarchy() []Symbol {
	return []Symbol{VectorSymbol, ArraySymbol, SequenceSymbol, TrueSymbol}
}

// SequenceType returns 'vector.
func (obj *Vector) SequenceType() Symbol {
	return VectorSymbol
}

// Length returns the length of the object.
func (obj *Vector) Length() int {
	return len(obj.elements)
}

// ArrayType returns 'vector.
func (obj *Vector) ArrayType() Symbol {
	return VectorSymbol
}

// Eval returns self.
func (obj *Vector) Eval(s *Scope, depth int) Object {
	return obj
}

func (obj *Vector) Push(ex int, values ...Object) (index int) {
	for _, v := range values {
		checkArrayElementType(v, obj.elementType)
		if 0 <= obj.FillPtr {
			index = obj.FillPtr
		} else {
			index = len(obj.elements)
		}
		if len(obj.elements) <= index {
			if ex <= 0 {
				return 0 // indicates not added
			}

			// TBD expand slice, try cap first

		}

		// TBD set the element at index
		// increment index

		// use fill pointer if not negative
		// if at end, append if adjustable else nothing

	}
	return
}

// AsList the Object into set of nested lists.
func (obj *Vector) AsList() List {
	return obj.elements
}
