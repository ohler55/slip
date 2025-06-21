// Copyright (c) 2025, Peter Ohler, All rights reserved.

package slip

import (
	"bytes"
	"math/big"
)

// UnsignedByteSymbol is the symbol with a value of "unsigned-byte".
const UnsignedByteSymbol = Symbol("unsigned-byte")

func init() {
	DefConstant(&CLPkg, string(UnsignedByteSymbol), UnsignedByteSymbol,
		`A _unsigned-byte_ is an integer with a specific range defined by the number of bits in the byte.`)
}

// UnsignedByte represents an integer with a specific number of bits. The type
// is a struct so that growing the bytes does not cause a new instance to be
// created.
type UnsignedByte struct {
	Bytes []byte // low bits at the end, big-endian ununsigned
}

// Size of the byte is bits.
func (obj *UnsignedByte) Size() int {
	return len(obj.Bytes) * 8
}

// String representation of the Object.
func (obj *UnsignedByte) String() string {
	return string(obj.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (obj *UnsignedByte) Append(b []byte) []byte {
	return printer.Append(b, obj.AsFixOrBig(), 0)
}

// Simplify the Object into an int64.
func (obj *UnsignedByte) Simplify() any {
	return obj.AsFixOrBig().Simplify()
}

// Equal returns true if this Object and the other are equal in value.
func (obj *UnsignedByte) Equal(other Object) (eq bool) {
	if ub, ok := other.(*UnsignedByte); ok {
		return bytes.Equal(obj.Bytes, ub.Bytes)
	}
	return obj.AsFixOrBig().Equal(other)
}

// Hierarchy returns the class hierarchy as symbols for the instance.
func (obj *UnsignedByte) Hierarchy() []Symbol {
	return []Symbol{
		UnsignedByteSymbol,
		SignedByteSymbol,
		IntegerSymbol,
		RationalSymbol,
		RealSymbol,
		NumberSymbol,
		TrueSymbol,
	}
}

// IntegerType returns 'unsigned-byte.
func (obj *UnsignedByte) IntegerType() Symbol {
	return UnsignedByteSymbol
}

// RationalType returns 'unsigned-byte.
func (obj *UnsignedByte) RationalType() Symbol {
	return UnsignedByteSymbol
}

// RealType returns 'unsigned-byte.
func (obj *UnsignedByte) RealType() Symbol {
	return UnsignedByteSymbol
}

// NumberType returns 'unsigned-byte.
func (obj *UnsignedByte) NumberType() Symbol {
	return UnsignedByteSymbol
}

// Eval returns self.
func (obj *UnsignedByte) Eval(s *Scope, depth int) Object {
	return obj
}

// RealValue of the number as a float64.
func (obj *UnsignedByte) RealValue() (rv float64) {
	switch ti := obj.AsFixOrBig().(type) {
	case Fixnum:
		rv = ti.RealValue()
	case *Bignum:
		rv = ti.RealValue()
	}
	return
}

// Dup returns a copy of the instance.
func (obj *UnsignedByte) Dup() *UnsignedByte {
	bytes := make([]byte, len(obj.Bytes))
	copy(bytes, obj.Bytes)
	return &UnsignedByte{Bytes: bytes}
}

// IsInt64 returns true if the instance can be represented by an int64.
func (obj *UnsignedByte) IsInt64() (ii bool) {
	switch ti := obj.AsFixOrBig().(type) {
	case Fixnum:
		ii = ti.IsInt64()
	case *Bignum:
		ii = ti.IsInt64()
	}
	return
}

// Int64 of the number.
func (obj *UnsignedByte) Int64() (i int64) {
	switch ti := obj.AsFixOrBig().(type) {
	case Fixnum:
		i = ti.Int64()
	case *Bignum:
		i = ti.Int64()
	}
	return
}

// AsFixOrBig returns the fixnum or bignum equivalent.
func (obj *UnsignedByte) AsFixOrBig() Object {
	if len(obj.Bytes) <= 8 && (obj.Bytes[0]&0x80) == 0 {
		var i64 int64
		for _, b := range obj.Bytes {
			i64 = i64<<8 | int64(b)
		}
		return Fixnum(i64)
	}
	var bi big.Int
	_ = bi.SetBytes(obj.Bytes)

	return (*Bignum)(&bi)
}

// SetBit sets the bit at index to the value specified.
func (obj *UnsignedByte) SetBit(index uint, value bool) {
	if len(obj.Bytes)*8 <= int(index) {
		obj.grow(index - uint(len(obj.Bytes)*8))
	}
	bi := len(obj.Bytes) - 1 - int(index)/8
	r := index % 8
	if value {
		obj.Bytes[bi] |= 1 << r
	} else {
		obj.Bytes[bi] &= ^(1 << r)
	}
}

// GetBit gets the bit at index and returns a boolean value.
func (obj *UnsignedByte) GetBit(index uint) (value bool) {
	if index < uint(len(obj.Bytes)*8) {
		bi := len(obj.Bytes) - 1 - int(index)/8
		r := index % 8
		value = (obj.Bytes[bi] & (1 << r)) != 0
	}
	return
}

func (obj *UnsignedByte) grow(cnt uint) {
	// Grow on the high side so bits have to be shifted right.
	cnt = cnt/8 + 1
	obj.Bytes = append(bytes.Repeat([]byte{0x00}, int(cnt)), obj.Bytes...)
}

// Invert the bits or NOT (complement) of each bit.
func (obj *UnsignedByte) Invert() {
	for i, b := range obj.Bytes {
		obj.Bytes[i] = ^b
	}
}
