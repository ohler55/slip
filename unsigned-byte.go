// Copyright (c) 2025, Peter Ohler, All rights reserved.

package slip

import (
	"bytes"
	"math/big"
)

// UnsignedByteSymbol is the symbol with a value of "unsigned-byte".
const UnsignedByteSymbol = Symbol("unsigned-byte")

func init() {
	DefConstant(UnsignedByteSymbol, UnsignedByteSymbol,
		`A _unsigned-byte_ is an integer with a specific range defined by the number of bits in the byte.`)
}

// UnsignedByte represents an integer with a specific number of bits.
type UnsignedByte struct {
	Bytes []byte // low bits at the end, big-endian ununsigned
	Size  uint
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
		return obj.Size == ub.Size && bytes.Equal(obj.Bytes, ub.Bytes)
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
	if obj.Size <= 64 {
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
