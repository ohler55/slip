// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import (
	"fmt"
)

// ArraySymbol is the symbol with a value of "array".
const (
	ArraySymbol       = Symbol("array")
	ArrayMaxRank      = 1024
	ArrayMaxDimension = 0x10000000
)

// Array is an n dimensional collection of Objects.
type Array struct {
	dims        []int
	sizes       []int
	elements    []Object
	elementType Symbol // defaults to TrueSymbol which is empty string
	adjustable  bool
}

// NewArray creates a new array with the specified dimensions and initial
// value.
func NewArray(
	dimensions []int,
	elementType Symbol,
	initElement Object,
	initContent List,
	adjustable bool) *Array {

	a := Array{
		dims:        dimensions,
		sizes:       make([]int, len(dimensions)),
		elementType: elementType,
		adjustable:  adjustable,
	}
	size := 1
	for i := len(dimensions) - 1; 0 <= i; i-- {
		a.sizes[i] = size
		if ArrayMaxDimension < dimensions[i] {
			PanicType("dimension", Fixnum(dimensions[i]),
				fmt.Sprintf("positive fixnum less than %d", ArrayMaxDimension))
		}
		size *= dimensions[i]
	}
	a.elements = make([]Object, size)
	if initContent != nil {
		a.SetAll(initContent)
	} else if initElement != nil {
		for i := len(a.elements) - 1; 0 <= i; i-- {
			a.elements[i] = initElement
		}
	}
	return &a
}

// ArrayType returns 'array.
func (obj *Array) ArrayType() Symbol {
	return ArraySymbol
}

// Assumes dims and size arrays already allocated. Used by the reader.
func (obj *Array) calcAndSet(list List) {
	if 0 < len(obj.dims) {
		orig := list
		var ok bool
		size := 1
		for i := 0; i < len(obj.dims); i++ {
			obj.dims[i] = len(list)
			obj.sizes[i] = size
			size *= len(list)
			if i < len(obj.dims)-1 {
				if list, ok = list[0].(List); !ok {
					NewPanic("Invalid data for a %d dimension array. %s", len(obj.dims), list)
				}
			}
		}
		obj.elements = make([]Object, size)
		obj.setDim(orig, 0, 0)
	}
}

// String representation of the Object.
func (obj *Array) String() string {
	return string(obj.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (obj *Array) Append(b []byte) []byte {
	return printer.Append(b, obj, 0)
}

// Simplify the Object into set of nested slices.
func (obj *Array) Simplify() any {
	list, _ := obj.simplifyDim(0, 0)
	return list
}

func (obj *Array) simplifyDim(di, ei int) ([]any, int) {
	d := obj.dims[di]
	list := make([]any, d)
	if di == len(obj.dims)-1 {
		for i := 0; i < d; i++ {
			e := obj.elements[ei]
			if e == nil {
				list[i] = nil
			} else {
				list[i] = e.Simplify()
			}
			ei++
		}
	} else {
		d2 := di + 1
		for i := 0; i < d; i++ {
			list[i], ei = obj.simplifyDim(d2, ei)
		}
	}
	return list, ei
}

// Equal returns true if this Object and the other are equal in value.
func (obj *Array) Equal(other Object) (eq bool) {
	to, ok := other.(*Array)
	if !ok {
		return false
	}
	if len(obj.dims) != len(to.dims) || len(obj.elements) != len(to.elements) {
		return false
	}
	for i, d := range obj.dims {
		if d != to.dims[i] {
			return false
		}
	}
	for i, e := range obj.elements {
		if !ObjectEqual(e, to.elements[i]) {
			return false
		}
	}
	return true
}

// Hierarchy returns the class hierarchy as symbols for the instance.
func (obj *Array) Hierarchy() []Symbol {
	return []Symbol{ArraySymbol, TrueSymbol}
}

// Eval returns self.
func (obj *Array) Eval(s *Scope, depth int) Object {
	return obj
}

// Dimensions of the array.
func (obj *Array) Dimensions() []int {
	return obj.dims
}

// Length returns the length of the object.
func (obj *Array) Length() int {
	return len(obj.elements)
}

// Get the value at the location identified by the indexes.
func (obj *Array) Get(indexes ...int) Object {
	return obj.elements[obj.MajorIndex(indexes...)]
}

// Set a value at the location identified by the indexes.
func (obj *Array) Set(value Object, indexes ...int) {
	pos := obj.MajorIndex(indexes...)
	obj.elements[pos] = Coerce(value, obj.elementType)
}

// MajorIndex for the indexes provided.
func (obj *Array) MajorIndex(indexes ...int) int {
	if len(indexes) != len(obj.dims) {
		NewPanic("Wrong number of subscripts, %d, for array of rank %d.", len(indexes), len(obj.dims))
	}
	pos := 0
	for i, index := range indexes {
		d := obj.dims[i]
		if index < 0 || d <= index {
			dims := make(List, len(obj.dims))
			for j, d2 := range obj.dims {
				dims[j] = Fixnum(d2)
			}
			NewPanic("Invalid index %d for axis %d of (array %s). Should be between 0 and %d.",
				index, i, dims, d)
		}
		pos += index * obj.sizes[i]
	}
	return pos
}

// MajorGet for the index provided.
func (obj *Array) MajorGet(index int) Object {
	if index < 0 || len(obj.elements) <= index {
		NewPanic("Invalid major index %d for (array %s). Should be between 0 and %d.", index, obj, len(obj.elements))
	}
	return obj.elements[index]
}

// MajorSet for the index provided.
func (obj *Array) MajorSet(index int, value Object) {
	if index < 0 || len(obj.elements) <= index {
		NewPanic("Invalid major index %d for (array %s). Should be between 0 and %d.", index, obj, len(obj.elements))
	}
	obj.elements[index] = value
}

// AsList the Object into set of nested lists.
func (obj *Array) AsList() (list List) {
	if 0 < len(obj.dims) {
		list, _ = obj.listifyDim(0, 0)
	}
	return
}

func (obj *Array) listifyDim(di, ei int) (List, int) {
	d := obj.dims[di]
	list := make(List, d)
	if di == len(obj.dims)-1 {
		for i := 0; i < d; i++ {
			e := obj.elements[ei]
			list[i] = e
			ei++
		}
	} else {
		d2 := di + 1
		for i := 0; i < d; i++ {
			list[i], ei = obj.listifyDim(d2, ei)
		}
	}
	return list, ei
}

// SetAll element from the nested list provided.
func (obj *Array) SetAll(all List) {
	if 0 < len(obj.dims) {
		_ = obj.setDim(all, 0, 0)
	}
}

func (obj *Array) setDim(list List, di, ei int) int {
	d := obj.dims[di]
	if d != len(list) {
		NewPanic("Malformed :initial-contents: Dimension of axis %d is %d but received %d.", di, d, len(list))
	}
	if di == len(obj.dims)-1 {
		for i := 0; i < d; i++ {
			obj.elements[ei] = Coerce(list[i], obj.elementType)
			ei++
		}
	} else {
		d2 := di + 1
		for i := 0; i < d; i++ {
			sub, ok := list[i].(List)
			if !ok {
				PanicType("array initial-content", list[i], "list")
			}
			ei = obj.setDim(sub, d2, ei)
		}
	}
	return ei
}

// Adjust array with new parameters.
func (obj *Array) Adjust(dimensions []int, elementType Symbol, initElement Object, initContent List) *Array {
	if len(dimensions) != len(obj.dims) {
		NewPanic("Expected %d new dimensions for array %s, but received %d.", len(obj.dims), obj, len(dimensions))
	}
	adjustable := obj.adjustable
	if !adjustable {
		dup := *obj
		dup.adjustable = true
		dup.elements = make(List, len(obj.elements))
		copy(dup.elements, obj.elements)
		dup.dims = make([]int, len(obj.dims))
		copy(dup.dims, obj.dims)
		dup.sizes = make([]int, len(obj.sizes))
		copy(dup.sizes, obj.sizes)
		obj = &dup
	}
	if initContent != nil { // start over
		obj.dims = dimensions
		obj.sizes = make([]int, len(dimensions))
		obj.elementType = elementType
		size := 1
		for i := len(dimensions) - 1; 0 <= i; i-- {
			obj.sizes[i] = size
			size *= dimensions[i]
		}
		obj.elements = make([]Object, size)
		obj.SetAll(initContent)
		return obj
	}
	if TrueSymbol != elementType {
		for _, v := range obj.elements {
			_ = Coerce(v, elementType)
		}
	}
	tmp := NewArray(dimensions, elementType, initElement, nil, true)
	index := make([]int, len(obj.dims))
top:
	for pos := len(tmp.elements) - 1; 0 <= pos; pos-- {
		off := pos
		for i, sz := range tmp.sizes {
			index[i] = off / sz
			off %= sz
		}
		opos := 0
		for i, idx := range index {
			d := obj.dims[i]
			if d <= idx {
				tmp.elements[pos] = initElement
				continue top
			}
			opos += idx * obj.sizes[i]
		}
		tmp.elements[pos] = obj.elements[opos]
	}
	obj.dims = tmp.dims
	obj.sizes = tmp.sizes
	obj.elements = tmp.elements
	obj.elementType = elementType
	obj.adjustable = adjustable

	return obj
}

// Rank of the array is returned,
func (obj *Array) Rank() int {
	return len(obj.dims)
}

// Adjustable returns true if the array is adjustable.
func (obj *Array) Adjustable() bool {
	return obj.adjustable
}

// ElementType returns the element-type of the array.
func (obj *Array) ElementType() Symbol {
	return obj.elementType
}

// SetElementType returns the element-type of the array.
func (obj *Array) SetElementType(ts Object) {
	sym, ok := ts.(Symbol)
	if !ok {
		PanicType("type-specification", ts, "symbol")
	}
	for _, v := range obj.elements {
		_ = Coerce(v, sym)
	}
	obj.elementType = sym
}

// Elements returns the elements of the array.
func (obj *Array) Elements() []Object {
	return obj.elements
}
