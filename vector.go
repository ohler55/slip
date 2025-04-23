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
func NewVector(dim int, elementType Symbol, initElement Object, elements List, adjustable bool) *Vector {
	if elements == nil {
		elements = make(List, dim)
		for i := dim - 1; 0 <= i; i-- {
			elements[i] = initElement
		}
	}
	return &Vector{
		Array: Array{
			dims:        []int{dim},
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
	elements := obj.elements
	if 0 <= obj.FillPtr && obj.FillPtr < len(elements) {
		elements = elements[:obj.FillPtr]
	}
	for i, v := range elements {
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

// Simplify the Object into a []any.
func (obj *Vector) Simplify() any {
	out := make([]any, len(obj.elements))
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

// ArrayType returns 'vector.
func (obj *Vector) ArrayType() Symbol {
	return VectorSymbol
}

// Eval returns self.
func (obj *Vector) Eval(s *Scope, depth int) Object {
	return obj
}

// Push a value onto the vector.
func (obj *Vector) Push(values ...Object) (index int) {
	for _, v := range values {
		v = Coerce(v, obj.elementType)
		if 0 <= obj.FillPtr {
			if len(obj.elements) <= obj.FillPtr {
				obj.elements = append(obj.elements, v)
				obj.FillPtr = len(obj.elements)
			} else {
				obj.elements[obj.FillPtr] = v
				obj.FillPtr++
			}
			index = obj.FillPtr
		} else {
			obj.elements = append(obj.elements, v)
			index = len(obj.elements)
		}
	}
	return
}

// Pop a value from the vector. The vector elements are not changed if there
// is a fill-pointer. If there is no fill pointer then the length of the
// vector is shortened by one.
func (obj *Vector) Pop() (element Object) {
	if 0 <= obj.FillPtr {
		if 0 < obj.FillPtr {
			obj.FillPtr--
			element = obj.elements[obj.FillPtr]
		}
	} else if 0 < len(obj.elements) {
		element = obj.elements[len(obj.elements)-1]
		obj.elements = obj.elements[:len(obj.elements)-1]
	}
	return
}

// AsList the Object into set of nested lists.
func (obj *Vector) AsList() List {
	if 0 <= obj.FillPtr && obj.FillPtr < len(obj.elements) {
		return obj.elements[:obj.FillPtr]
	}
	return obj.elements
}

// Adjust array with new parameters.
func (obj *Vector) Adjust(dims []int, elementType Symbol, initElement Object, initContent List, fillPtr int) VectorLike {
	if len(dims) != len(obj.dims) {
		NewPanic("Expected %d new dimensions for array %s, but received %d.", len(obj.dims), obj, len(dims))
	}
	if !obj.adjustable {
		if initContent == nil {
			content := make(List, dims[0])
			copy(content, obj.elements)
			for i := len(obj.elements); i < len(content); i++ {
				content[i] = initElement
			}
			initContent = content
		}
		vv := NewVector(dims[0], elementType, initElement, initContent, false)
		vv.FillPtr = fillPtr
		return vv
	}
	obj.Array.Adjust(dims, elementType, initElement, initContent)
	obj.FillPtr = fillPtr

	return obj
}

// FillPointer returns the fill-pointer as an int.
func (obj *Vector) FillPointer() int {
	return obj.FillPtr
}

// SetFillPointer sets the fill-pointer.
func (obj *Vector) SetFillPointer(fp int) {
	if fp < 0 || len(obj.elements) <= fp {
		NewPanic("Invalid major index %d for (array %s). Should be between 0 and %d.", fp, obj, len(obj.elements))
	}
	obj.FillPtr = fp
}
