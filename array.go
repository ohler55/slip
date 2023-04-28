// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import (
	"fmt"
	"math"
)

// ArraySymbol is the symbol with a value of "array".
const (
	ArraySymbol  = Symbol("array")
	ArrayMaxRank = 1024
)

func init() {
	DefConstant(ArraySymbol, ArraySymbol,
		`An _array_ is an _n_ dimensional collection of _objects_ identified by a _fixnum_ indices on each dimension.`)

	// Somewhat arbitrary. Could be anything.
	DefConstant(Symbol("array-rank-limit"), Fixnum(ArrayMaxRank), `The upper bound on the rank of an _array_.`)
	DefConstant(Symbol("array-total-size-limit"), Fixnum(math.MaxInt), `The upper bound on the size of an _array_.`)
}

// Array is an n dimensional collection of Objects.
type Array struct {
	dims     []int
	sizes    []int
	elements []Object
}

// NewArray creates a new array with the specified dimensions and initial
// value.
func NewArray(initVal Object, dimensions ...int) *Array {
	a := Array{
		dims:  dimensions,
		sizes: make([]int, len(dimensions)),
	}
	size := 1
	for i := len(dimensions) - 1; 0 <= i; i-- {
		a.sizes[i] = size
		size *= dimensions[i]
	}
	a.elements = make([]Object, size)
	if initVal != nil {
		for i := len(a.elements) - 1; 0 <= i; i-- {
			a.elements[i] = initVal
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
				if list, ok = list[0].(List); !ok { // TBD last?
					panic(fmt.Sprintf("Invalid data for a %d dimension array. %s", len(obj.dims), list))
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
	if len(indexes) != len(obj.dims) {
		panic(fmt.Sprintf("Wrong number of subscripts, %d, for array of rank %d.", len(indexes), len(obj.dims)))
	}
	pos := 0
	for i, index := range indexes {
		d := obj.dims[i]
		if index < 0 || d <= index {
			dims := make(List, len(obj.dims))
			for j, d2 := range obj.dims {
				dims[j] = Fixnum(d2)
			}
			panic(fmt.Sprintf("Invalid index %d for axis %d of (array %s). Should be between 0 and %d.",
				index, i, dims, d))
		}
		pos += index * obj.sizes[i]
	}
	return obj.elements[pos]
}

// Set a value at the location identified by the indexes.
func (obj *Array) Set(value Object, indexes ...int) {
	if len(indexes) != len(obj.dims) {
		panic(fmt.Sprintf("Wrong number of subscripts, %d, for array of rank %d.", len(indexes), len(obj.dims)))
	}
	pos := 0
	for i, index := range indexes {
		d := obj.dims[i]
		if index < 0 || d <= index {
			dims := make(List, len(obj.dims))
			for j, d2 := range obj.dims {
				dims[j] = Fixnum(d2)
			}
			panic(fmt.Sprintf("Invalid index %d for axis %d of (array %s). Should be between 0 and %d.",
				index, i, dims, d))
		}
		pos += index * obj.sizes[i]
	}
	obj.elements[pos] = value
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
		panic(fmt.Sprintf("Malformed :initial-contents: Dimension of axis %d is %d but received %d long.",
			di, d, len(list)))
	}
	if di == len(obj.dims)-1 {
		for i := 0; i < d; i++ {
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

// Resize the array using the initVal as the value for new elements.
func (obj *Array) Resize(initVal Object, dimensions ...int) {
	// TBD
}
