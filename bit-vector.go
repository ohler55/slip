// Copyright (c) 2025, Peter Ohler, All rights reserved.

package slip

import (
	"bytes"
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
	Bytes      []byte
	Size       uint
	FillPtr    int
	Adjustable bool
}

// ReadBitVector parses a sequence of 0 and 1 to form a bit-vector.
func ReadBitVector(b []byte) *BitVector {
	bv := BitVector{FillPtr: -1}
	bv.Size = uint(len(b))
	if 0 < len(b) {
		bv.Bytes = make([]byte, len(b)/8+1)
		for i, v := range b {
			bv.Set(uint(i), v != '0')
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
	size := int(obj.Size)
	if 0 < obj.FillPtr {
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
		eq = obj.Size == bv.Size && obj.FillPtr == bv.FillPtr && bytes.Equal(obj.Bytes, bv.Bytes)
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
	return int(obj.Size)
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
	if pos < obj.Size {
		b := obj.Bytes[pos/8]
		r := pos % 8
		if (b>>(7-r))&0x01 == 1 {
			return true
		}
	}
	return false
}

// Set the bit at pos to the value.
func (obj *BitVector) Set(pos uint, value bool) {
	if pos < obj.Size {
		r := pos % 8
		if value {
			obj.Bytes[pos/8] |= 0x01 << (7 - r)
		} else {
			obj.Bytes[pos/8] &= ^(0x01 << (7 - r))
		}
		return
	}
	NewPanic("Invalid major index %d for (bit-vector %s). Should be between 0 and %d.", pos, obj, obj.Size)
}

func (obj *BitVector) grow(cnt int) {
	obj.Size += uint(cnt)
	if len(obj.Bytes)*8 < int(obj.Size) {
		obj.Bytes = append(obj.Bytes, bytes.Repeat([]byte{0x00}, cnt)...)
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
	loc := obj.Size
	if obj.FillPtr < 0 {
		obj.grow(len(values))
	} else {
		if int(obj.Size) <= obj.FillPtr+len(values) {
			obj.grow(obj.FillPtr + len(values) - int(obj.Size))
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
		obj.Set(loc, b)
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
	} else if 0 < obj.Size {
		if obj.At(obj.Size - 1) {
			element = Bit(1)
		} else {
			element = Bit(0)
		}
		obj.Size--
	}
	return
}

// // AsList the Object into set of nested lists.
// func (obj *Vector) AsList() List {
// 	if 0 <= obj.FillPtr && obj.FillPtr < len(obj.elements) {
// 		return obj.elements[:obj.FillPtr]
// 	}
// 	return obj.elements
// }

// // Adjust array with new parameters.
// func (obj *Vector) Adjust(dims []int, elementType Symbol, initElement Object, initContent List, fillPtr int) *Vector {
// 	if len(dims) != len(obj.dims) {
// 		NewPanic("Expected %d new dimensions for array %s, but received %d.", len(obj.dims), obj, len(dims))
// 	}
// 	if !obj.adjustable {
// 		if initContent == nil {
// 			content := make(List, dims[0])
// 			copy(content, obj.elements)
// 			for i := len(obj.elements); i < len(content); i++ {
// 				content[i] = initElement
// 			}
// 			initContent = content
// 		}
// 		vv := NewVector(dims[0], elementType, initElement, initContent, false)
// 		vv.FillPtr = fillPtr
// 		return vv
// 	}
// 	obj.Array.Adjust(dims, elementType, initElement, initContent)
// 	obj.FillPtr = fillPtr

// 	return obj
// }
