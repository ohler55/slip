// Copyright (c) 2024, Peter Ohler, All rights reserved.

package slip

import "bytes"

// OctetsSymbol is the symbol with a value of "octets".
const OctetsSymbol = Symbol("octets")

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

// Rank of the array is returned,
func (obj Octets) Rank() int {
	return 1
}

// Adjustable returns true if the array is adjustable.
func (obj Octets) Adjustable() bool {
	return false
}

// ElementType returns the element-type of the array.
func (obj Octets) ElementType() Symbol {
	return OctetSymbol
}

// SetElementType sets the element-type of the array.
func (obj Octets) SetElementType(ts Object) {
	if ts != OctetSymbol {
		PanicType("octets element", ts, "octet")
	}
}

// Dimensions of the array.
func (obj Octets) Dimensions() []int {
	return []int{len(obj)}
}

// Adjust array with new parameters.
func (obj Octets) Adjust(dims []int, eType Symbol, initVal Object, initContent List, fillPtr int) VectorLike {
	if len(dims) != 1 {
		NewPanic("Expected 1 new dimensions for a octets %s, but received %d.", obj, len(dims))
	}
	if eType != OctetSymbol {
		PanicType(":element-type", eType, "octet")
	}
	var iv byte
	if initVal != nil {
		if num, ok := initVal.(Integer); ok && num.IsInt64() && 0 <= num.Int64() && num.Int64() < 256 {
			iv = byte(num.Int64())
		} else {
			PanicType(":initial-element", initVal, "nil", "integer between 0 and 256")
		}
	}
	if initContent != nil {
		if len(initContent) != dims[0] {
			NewPanic("Malformed :initial-contents. Dimensions on axis 0 is %d but initial-content length is %d.",
				dims[0], len(initContent))
		}
		for _, v := range initContent {
			if vi, ok := v.(Integer); !ok || !vi.IsInt64() || vi.Int64() < 0 || 255 < vi.Int64() {
				PanicType(":initial-contents", v, "nil", "list of integers between 0 and 256")
			}
		}
	}
	size := len(obj)
	dup := make(Octets, dims[0])
	copy(dup, obj)
	obj = dup

	if 0 < len(initContent) {
		for i, v := range initContent {
			obj[i] = byte(v.(Integer).Int64())
		}
	} else if initVal != nil {
		for ; size < len(obj); size++ {
			obj[size] = iv
		}
	}
	return obj
}

// Get the value at the location identified by the indexes.
func (obj Octets) Get(indexes ...int) Object {
	if len(indexes) != 1 {
		NewPanic("Wrong number of subscripts, %d, for octets of rank 1.", len(indexes))
	}
	pos := indexes[0]
	if pos < 0 || len(obj) <= pos {
		NewPanic("Invalid index %d for axis 1 of bit-vector. Should be between 0 and %d.", pos, len(obj))
	}
	return Octet(obj[pos])
}

// Set a value at the location identified by the indexes.
func (obj Octets) Set(value Object, indexes ...int) {
	if len(indexes) != 1 {
		NewPanic("Wrong number of subscripts, %d, for octets of rank 1.", len(indexes))
	}
	pos := indexes[0]
	if pos < 0 || len(obj) <= pos {
		NewPanic("Invalid index %d for axis 1 of bit-vector. Should be between 0 and %d.", pos, len(obj))
	}
	if num, ok := value.(Integer); ok && num.IsInt64() && 0 <= num.Int64() && num.Int64() < 256 {
		obj[pos] = byte(num.Int64())
	} else {
		PanicType("set value", value, "integer between 0 and 256")
	}
}

// MajorIndex for the indexes provided.
func (obj Octets) MajorIndex(indexes ...int) int {
	if len(indexes) != 1 {
		NewPanic("Wrong number of subscripts, %d, for array of rank 1.", len(indexes))
	}
	pos := indexes[0]
	if pos < 0 || len(obj) <= pos {
		NewPanic("Invalid index %d for axis 1 of bit-vector. Should be between 0 and %d.", pos, len(obj))
	}
	return pos
}

// MajorGet for the index provided.
func (obj Octets) MajorGet(index int) Object {
	if index < 0 || len(obj) <= index {
		NewPanic("Invalid major index %d for (array %s). Should be between 0 and %d.", index, obj, len(obj))
	}
	return obj.Get(index)
}

// MajorSet for the index provided.
func (obj Octets) MajorSet(index int, value Object) {
	if index < 0 || len(obj) <= index {
		NewPanic("Invalid major index %d for (array %s). Should be between 0 and %d.", index, obj, len(obj))
	}
	obj.Set(value, index)
}

// LoadForm returns a form that can be evaluated to create the object or nil
// if that is not possible.
func (obj Octets) LoadForm() Object {
	list := make(List, len(obj)+1)
	list[0] = Symbol("list")
	for i, o := range obj {
		list[i+1] = Fixnum(o)
	}
	return List{Symbol("coerce"), list, List{Symbol("quote"), Symbol("octets")}}
}
