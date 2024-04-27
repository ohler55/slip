// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import (
	"fmt"
	"math"
)

// ArraySymbol is the symbol with a value of "array".
const (
	ArraySymbol       = Symbol("array")
	ArrayMaxRank      = 1024
	ArrayMaxDimension = 0x10000000
)

func init() {
	DefConstant(ArraySymbol, ArraySymbol,
		`An _array_ is an _n_ dimensional collection of _objects_ identified by a _fixnum_ indices on each dimension.`)

	// Somewhat arbitrary. Could be anything.
	DefConstant(Symbol("array-rank-limit"), Fixnum(ArrayMaxRank), `The upper bound on the rank of an _array_.`)
	DefConstant(Symbol("array-dimension-limit"), Fixnum(ArrayMaxDimension),
		`The upper exclusive bound on each dimension of an _array_.`)
	DefConstant(Symbol("array-total-size-limit"), Fixnum(math.MaxInt), `The upper bound on the size of an _array_.`)
}

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
func (obj *Array) Simplify() interface{} {
	list, _ := obj.simplifyDim(0, 0)
	return list
}

func (obj *Array) simplifyDim(di, ei int) ([]interface{}, int) {
	d := obj.dims[di]
	list := make([]interface{}, d)
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

// Size of the array.
func (obj *Array) Size() int {
	return len(obj.elements)
}

// Get the value at the location identified by the indexes.
func (obj *Array) Get(indexes ...int) Object {
	return obj.elements[obj.MajorIndex(indexes...)]
}

// Set a value at the location identified by the indexes.
func (obj *Array) Set(value Object, indexes ...int) {
	pos := obj.MajorIndex(indexes...)
	checkArrayElementType(value, obj.elementType)
	obj.elements[pos] = value
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
			checkArrayElementType(list[i], obj.elementType)
			obj.elements[ei] = list[i]
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
	if !obj.adjustable {
		if initContent == nil {
			// content := make(List, dims[0])
			// copy(content, obj.elements)
			// for i := len(obj.elements); i < len(content); i++ {
			// 	content[i] = initElement
			// }
			// initContent = content

			// TBD form new contents from old but add new element types as needed
		}
		return NewArray(dimensions, elementType, initElement, initContent, false)
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
			checkArrayElementType(v, elementType)
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

func checkArrayElementType(v Object, et Symbol) {
	if v != nil && 0 < len(et) {
		for _, sym := range v.Hierarchy() {
			if sym == et {
				return
			}
		}
		PanicType("array element", v, string(et))
	}
}
