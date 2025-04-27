// Copyright (c) 2025, Peter Ohler, All rights reserved.

package slip

import (
	"bytes"
	"math/big"
)

// BitVectorSymbol is the symbol with a value of "bit-vector".
const BitVectorSymbol = Symbol("bit-vector")

func init() {
	DefConstant(BitVectorSymbol, BitVectorSymbol, `A _bit-vector_ is a vector of bits.`)
}

// BitVector represents an integer with a specific number of bits.
type BitVector struct {
	// Bytes in left to right order, the last byte includes padding on the low
	// bits if needed.
	Bytes     []byte
	Len       uint
	FillPtr   int
	CanAdjust bool
}

// ReadBitVector parses a sequence of 0 and 1 to form a bit-vector.
func ReadBitVector(b []byte) *BitVector {
	bv := BitVector{FillPtr: -1}
	bv.Len = uint(len(b))
	if 0 < len(b) {
		bv.Bytes = make([]byte, len(b)/8+1)
		for i, v := range b {
			bv.Put(uint(i), v != '0')
		}
	}
	return &bv
}

// String representation of the Object.
func (obj *BitVector) String() string {
	return string(obj.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (obj *BitVector) Append(b []byte) []byte {
	b = append(b, '#', '*')
	size := int(obj.Len)
	if 0 <= obj.FillPtr && obj.FillPtr < int(obj.Len) {
		size = obj.FillPtr
	}
	last := size / 8
	for i, bb := range obj.Bytes {
		if i < last {
			for i := 7; 0 <= i; i-- {
				if (bb>>i)&0x01 == 0 {
					b = append(b, '0')
				} else {
					b = append(b, '1')
				}
			}
		} else {
			end := size % 8
			for i := 0; i < end; i++ {
				if (bb>>(7-i))&0x01 == 0 {
					b = append(b, '0')
				} else {
					b = append(b, '1')
				}
			}
			break
		}
	}
	return b
}

// Simplify the Object into an int64.
func (obj *BitVector) Simplify() any {
	return obj.String()
}

// Equal returns true if this Object and the other are equal in value.
func (obj *BitVector) Equal(other Object) (eq bool) {
	if bv, ok := other.(*BitVector); ok {
		eq = obj.Len == bv.Len && obj.FillPtr == bv.FillPtr && bytes.Equal(obj.Bytes, bv.Bytes)
	}
	return
}

// Hierarchy returns the class hierarchy as symbols for the instance.
func (obj *BitVector) Hierarchy() []Symbol {
	return []Symbol{BitVectorSymbol, VectorSymbol, ArraySymbol, SequenceSymbol, TrueSymbol}
}

// SequenceType() Symbol returns 'bit-vector.
func (obj *BitVector) SequenceType() Symbol {
	return BitVectorSymbol
}

// Length returns the length of the object.
func (obj *BitVector) Length() int {
	return int(obj.Len)
}

// ArrayType returns 'bit-vector.
func (obj *BitVector) ArrayType() Symbol {
	return BitVectorSymbol
}

// Eval returns self.
func (obj *BitVector) Eval(s *Scope, depth int) Object {
	return obj
}

// At returns true if the bit at pos is set or false if not set.
func (obj *BitVector) At(pos uint) bool {
	if pos < obj.Len {
		b := obj.Bytes[pos/8]
		r := pos % 8
		if (b>>(7-r))&0x01 == 1 {
			return true
		}
	}
	return false
}

// Get the value at the location identified by the indexes.
func (obj *BitVector) Get(indexes ...int) Object {
	if len(indexes) != 1 {
		NewPanic("Wrong number of subscripts, %d, for bit-vector of rank 1.", len(indexes))
	}
	pos := indexes[0]
	if pos < 0 || int(obj.Len) <= pos {
		NewPanic("Invalid index %d for axis 1 of bit-vector. Should be between 0 and %d.", pos, obj.Len)
	}
	if obj.At(uint(pos)) {
		return Bit(1)
	}
	return Bit(0)
}

// Put the bit at pos to the value.
func (obj *BitVector) Put(pos uint, value bool) {
	if pos < obj.Len {
		r := pos % 8
		if value {
			obj.Bytes[pos/8] |= 0x01 << (7 - r)
		} else {
			obj.Bytes[pos/8] &= ^(0x01 << (7 - r))
		}
		return
	}
	NewPanic("Invalid major index %d for (bit-vector %s). Should be between 0 and %d.", pos, obj, obj.Len)
}

// Set a value at the location identified by the indexes.
func (obj *BitVector) Set(value Object, indexes ...int) {
	if len(indexes) != 1 {
		NewPanic("Wrong number of subscripts, %d, for bit-vector of rank 1.", len(indexes))
	}
	pos := indexes[0]
	if pos < 0 || int(obj.Len) <= pos {
		NewPanic("Invalid index %d for axis 1 of bit-vector. Should be between 0 and %d.", pos, obj.Len)
	}
	if num, ok := value.(Integer); ok && num.IsInt64() {
		switch num.Int64() {
		case 0:
			obj.Put(uint(pos), false)
			return
		case 1:
			obj.Put(uint(pos), true)
			return
		}
	}
	PanicType("set value", value, "0", "1")
}

// Grow expands the bit-vector by the length specified.
func (obj *BitVector) Grow(cnt int) {
	obj.Len += uint(cnt)
	if len(obj.Bytes)*8 < int(obj.Len) {
		obj.Bytes = append(obj.Bytes, bytes.Repeat([]byte{0x00}, int(obj.Len)/8+1-len(obj.Bytes))...)
	}
}

// Push a value onto the vector.
func (obj *BitVector) Push(values ...Object) (index int) {
	// validate first
	for _, v := range values {
		switch v {
		case Fixnum(0), Bit(0), Fixnum(1), Bit(1):
			// okay
		default:
			PanicType("array element", v, "0", "1")
		}
	}
	loc := obj.Len
	if obj.FillPtr < 0 {
		obj.Grow(len(values))
	} else {
		if int(obj.Len) <= obj.FillPtr+len(values) {
			obj.Grow(obj.FillPtr + len(values) - int(obj.Len))
		}
		loc = uint(obj.FillPtr)
		obj.FillPtr += len(values)
	}
	for _, v := range values {
		var b bool
		switch v {
		case Fixnum(0), Bit(0):
			b = false
		case Fixnum(1), Bit(1):
			b = true
		}
		obj.Put(loc, b)
		loc++
	}
	return
}

// Pop a value from the vector. The vector elements are not changed if there
// is a fill-pointer. If there is no fill pointer then the length of the
// vector is shortened by one. Note a vector-pop without a fill pointer is not
// standard common lisp.
func (obj *BitVector) Pop() (element Object) {
	if 0 <= obj.FillPtr {
		if 0 < obj.FillPtr {
			obj.FillPtr--
			if obj.At(uint(obj.FillPtr)) {
				element = Bit(1)
			} else {
				element = Bit(0)
			}
		}
	} else if 0 < obj.Len {
		if obj.At(obj.Len - 1) {
			element = Bit(1)
		} else {
			element = Bit(0)
		}
		obj.Len--
	}
	return
}

// AsList the Object into set of nested lists.
func (obj *BitVector) AsList() List {
	size := int(obj.Len)
	if 0 <= obj.FillPtr && obj.FillPtr < int(obj.Len) {
		size = obj.FillPtr
	}
	last := size / 8
	list := make(List, 0, size)
	for i, bb := range obj.Bytes {
		if i < last {
			for i := 7; 0 <= i; i-- {
				if (bb>>i)&0x01 == 0 {
					list = append(list, Bit(0))
				} else {
					list = append(list, Bit(1))
				}
			}
		} else {
			end := size % 8
			for i := 0; i < end; i++ {
				if (bb>>(7-i))&0x01 == 0 {
					list = append(list, Bit(0))
				} else {
					list = append(list, Bit(1))
				}
			}
			break
		}
	}
	return list
}

// Adjust array with new parameters.
func (obj *BitVector) Adjust(dims []int, eType Symbol, initVal Object, initContent List, fillPtr int) VectorLike {
	if len(dims) != 1 {
		NewPanic("Expected 1 new dimensions for a bit-vector %s, but received %d.", obj, len(dims))
	}
	if eType != BitSymbol {
		PanicType(":element-type", eType, "bit")
	}
	var iv bool
	if initVal != nil {
		if num, ok := initVal.(Integer); ok && num.IsInt64() && (num.Int64() == 0 || num.Int64() == 1) {
			iv = num.Int64() != 0
		} else {
			PanicType(":initial-element", initVal, "nil", "0", "1")
		}
	}
	if initContent != nil {
		if len(initContent) != dims[0] {
			NewPanic("Malformed :initial-contents. Dimensions on axis 0 is %d but initial-content length is %d.",
				dims[0], len(initContent))
		}
		for _, v := range initContent {
			if vi, ok := v.(Integer); ok && vi.IsInt64() {
				switch vi.Int64() {
				case 0, 1:
					continue
				}
			}
			PanicType(":initial-contents", v, "0", "1")
		}
	}
	size := obj.Len
	if !obj.CanAdjust {
		bv := BitVector{
			Bytes:   make([]byte, dims[0]),
			Len:     uint(dims[0]),
			FillPtr: fillPtr,
		}
		copy(bv.Bytes, obj.Bytes)
		obj = &bv
	} else {
		switch {
		case dims[0] < int(obj.Len):
			obj.Len = uint(dims[0])
		case dims[0] > int(obj.Len):
			obj.Grow(dims[0] - int(obj.Len))
		}
		obj.FillPtr = fillPtr
	}
	if 0 < len(initContent) {
		for i, v := range initContent {
			obj.Put(uint(i), v.(Integer).Int64() != 0)
		}
	} else if initVal != nil {
		for ; size < obj.Len; size++ {
			obj.Put(size, iv)
		}
	}
	return obj
}

// Rank of the array is returned,
func (obj *BitVector) Rank() int {
	return 1
}

// Adjustable returns true if the array is adjustable.
func (obj *BitVector) Adjustable() bool {
	return obj.CanAdjust
}

// ElementType returns the element-type of the array.
func (obj *BitVector) ElementType() Symbol {
	return BitSymbol
}

// SetElementType sets the element-type of the bit-vector.
func (obj *BitVector) SetElementType(ts Object) {
	if ts != BitSymbol {
		PanicType("bit-vector element", ts, "bit")
	}
}

// Dimensions of the array.
func (obj *BitVector) Dimensions() []int {
	return []int{int(obj.Len)}
}

// FillPointer returns the fill-pointer as an int.
func (obj *BitVector) FillPointer() int {
	return obj.FillPtr
}

// MajorIndex for the indexes provided.
func (obj *BitVector) MajorIndex(indexes ...int) int {
	if len(indexes) != 1 {
		NewPanic("Wrong number of subscripts, %d, for array of rank 1.", len(indexes))
	}
	pos := indexes[0]
	if pos < 0 || int(obj.Len) <= pos {
		NewPanic("Invalid index %d for axis 1 of bit-vector. Should be between 0 and %d.", pos, obj.Len)
	}
	return pos
}

// MajorGet for the index provided.
func (obj *BitVector) MajorGet(index int) Object {
	if index < 0 || int(obj.Len) <= index {
		NewPanic("Invalid major index %d for (array %s). Should be between 0 and %d.", index, obj, obj.Len)
	}
	return obj.Get(index)
}

// MajorSet for the index provided.
func (obj *BitVector) MajorSet(index int, value Object) {
	if index < 0 || int(obj.Len) <= index {
		NewPanic("Invalid major index %d for (array %s). Should be between 0 and %d.", index, obj, obj.Len)
	}
	obj.Set(value, index)
}

// SetFillPointer sets the fill-pointer.
func (obj *BitVector) SetFillPointer(fp int) {
	if fp < 0 || int(obj.Len) <= fp {
		NewPanic("Invalid fill-pointer %d for (array %s). Should be between 0 and %d.", fp, obj, obj.Len)
	}
	obj.FillPtr = fp
}

// Duplicate the instance.
func (obj *BitVector) Duplicate() *BitVector {
	bv := BitVector{
		Bytes:     make([]byte, len(obj.Bytes)),
		Len:       obj.Len,
		FillPtr:   obj.FillPtr,
		CanAdjust: obj.CanAdjust,
	}
	copy(bv.Bytes, obj.Bytes)
	return &bv
}

// Reverse the bits.
func (obj *BitVector) Reverse() {
	tmp := make([]byte, len(obj.Bytes))
	last := int(obj.Len) - 1
	for i := 0; i <= last; i++ {
		b := obj.Bytes[i/8]
		r := i % 8
		j := last - i
		tr := j % 8
		tmp[j/8] |= ((b >> (7 - r)) & 0x01) << (7 - tr)
	}
	obj.Bytes = tmp
}

// AsFixnum returns a Fixnum with the same bits as the vector. If more than 64
// bits are in the vector then the first 64 bits are used to form the Fixnum
// and the boolean return is false.
func (obj *BitVector) AsFixnum() (Fixnum, bool) {
	var num Fixnum
	for i := uint(0); i < obj.Len; i++ {
		num = (num << 1)
		if obj.At(i) {
			num |= 1
		}
	}
	return num, obj.Len <= 64
}

// AsBignum returns the vector as a BigNum.
func (obj *BitVector) AsBignum() *Bignum {
	bi := big.NewInt(0)
	last := obj.Len - 1
	for i := uint(0); i < obj.Len; i++ {
		if obj.At(i) {
			bi.SetBit(bi, int(last-i), 1)
		} else {
			bi.SetBit(bi, int(last-i), 0)
		}
	}
	return (*Bignum)(bi)
}
