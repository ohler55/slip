// Copyright (c) 2025, Peter Ohler, All rights reserved.

package slip

import (
	"bytes"
	"math/big"
)

// SignedByteSymbol is the symbol with a value of "signed-byte".
const SignedByteSymbol = Symbol("signed-byte")

func init() {
	DefConstant(SignedByteSymbol, SignedByteSymbol,
		`A _signed-byte_ is an integer with a specific range defined by the number of bits in the byte.`)
}

// SignedByte represents an integer with a specific number of bits. The Bytes
// are the absolute value of the integer value of the instance.
type SignedByte struct {
	Bytes []byte // low bits at the end, big-endian unsigned
}

// SignedByteFromInt64 creates a new signed-byte from an int64.
func SignedByteFromInt64(i64 int64) *SignedByte {
	var u64 uint64
	if i64 < 0 {
		u64 = uint64(-i64)
	} else {
		u64 = uint64(i64)
	}
	sb := SignedByte{
		Bytes: []byte{
			byte(u64 >> 56),
			byte((u64 >> 48) & 0x00000000000000ff),
			byte((u64 >> 40) & 0x00000000000000ff),
			byte((u64 >> 32) & 0x00000000000000ff),
			byte((u64 >> 24) & 0x00000000000000ff),
			byte((u64 >> 16) & 0x00000000000000ff),
			byte((u64 >> 8) & 0x00000000000000ff),
			byte(u64 & 0x00000000000000ff),
		},
	}
	if i64 < 0 {
		sb.Neg()
	}
	return &sb
}

// SignedByteFromUint64 creates a new signed-byte from an uint64.
func SignedByteFromUint64(u64 uint64) *SignedByte {
	return &SignedByte{
		Bytes: []byte{
			byte(u64 >> 56),
			byte((u64 >> 48) & 0x00000000000000ff),
			byte((u64 >> 40) & 0x00000000000000ff),
			byte((u64 >> 32) & 0x00000000000000ff),
			byte((u64 >> 24) & 0x00000000000000ff),
			byte((u64 >> 16) & 0x00000000000000ff),
			byte((u64 >> 8) & 0x00000000000000ff),
			byte(u64 & 0x00000000000000ff),
		},
	}
}

// SignedByteFromBigInt creates a new signed-byte from an big.Int.
func SignedByteFromBigInt(bi *big.Int) *SignedByte {
	sb := SignedByte{Bytes: bi.Bytes()}
	switch bi.Sign() {
	case 0:
		sb.Bytes = []byte{0}
	case -1:
		sb.Neg()
	}
	return &sb
}

// Dup returns a copy of the instance.
func (obj *SignedByte) Dup() *SignedByte {
	bytes := make([]byte, len(obj.Bytes))
	copy(bytes, obj.Bytes)
	return &SignedByte{Bytes: bytes}
}

// Size of the byte is bits.
func (obj *SignedByte) Size() int {
	return len(obj.Bytes) * 8
}

// String representation of the Object.
func (obj *SignedByte) String() string {
	return string(obj.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (obj *SignedByte) Append(b []byte) []byte {
	return printer.Append(b, obj.AsFixOrBig(), 0)
}

// Simplify the Object into an int64.
func (obj *SignedByte) Simplify() any {
	return obj.AsFixOrBig().Simplify()
}

// Equal returns true if this Object and the other are equal in value.
func (obj *SignedByte) Equal(other Object) (eq bool) {
	if sb, ok := other.(*SignedByte); ok {
		return bytes.Equal(obj.Bytes, sb.Bytes)
	}
	return obj.AsFixOrBig().Equal(other)
}

// Hierarchy returns the class hierarchy as symbols for the instance.
func (obj *SignedByte) Hierarchy() []Symbol {
	return []Symbol{SignedByteSymbol, IntegerSymbol, RationalSymbol, RealSymbol, NumberSymbol, TrueSymbol}
}

// IntegerType returns 'signed-byte.
func (obj *SignedByte) IntegerType() Symbol {
	return SignedByteSymbol
}

// RationalType returns 'signed-byte.
func (obj *SignedByte) RationalType() Symbol {
	return SignedByteSymbol
}

// RealType returns 'signed-byte.
func (obj *SignedByte) RealType() Symbol {
	return SignedByteSymbol
}

// NumberType returns 'signed-byte.
func (obj *SignedByte) NumberType() Symbol {
	return SignedByteSymbol
}

// Eval returns self.
func (obj *SignedByte) Eval(s *Scope, depth int) Object {
	return obj
}

// RealValue of the number as a float64.
func (obj *SignedByte) RealValue() (rv float64) {
	switch ti := obj.AsFixOrBig().(type) {
	case Fixnum:
		rv = ti.RealValue()
	case *Bignum:
		rv = ti.RealValue()
	}
	return
}

// IsInt64 returns true if the instance can be represented by an int64.
func (obj *SignedByte) IsInt64() (ii bool) {
	switch ti := obj.AsFixOrBig().(type) {
	case Fixnum:
		ii = ti.IsInt64()
	case *Bignum:
		ii = ti.IsInt64()
	}
	return
}

// Int64 of the number.
func (obj *SignedByte) Int64() (i int64) {
	switch ti := obj.AsFixOrBig().(type) {
	case Fixnum:
		i = ti.Int64()
	case *Bignum:
		i = ti.Int64()
	}
	return
}

// AsFixOrBig returns the fixnum or bignum equivalent.
func (obj *SignedByte) AsFixOrBig() Object {
	if len(obj.Bytes) <= 8 {
		var u64 uint64
		if (obj.Bytes[0] & 0x80) != 0 {
			u64 = 0xffffffffffffffff
		}
		for _, b := range obj.Bytes {
			u64 = u64<<8 | uint64(b)
		}
		return Fixnum(u64)
	}
	var bi big.Int
	if (obj.Bytes[0] & 0x80) != 0 {
		sb := obj.Dup()
		sb.Neg()
		_ = bi.SetBytes(sb.Bytes)
		_ = bi.Neg(&bi)
	} else {
		_ = bi.SetBytes(obj.Bytes)
	}
	return (*Bignum)(&bi)
}

// IsNeg returns true if the instance is a negative value.
func (obj *SignedByte) IsNeg() bool {
	return (obj.Bytes[0] & 0x80) != 0
}

// Neg negates the value.
func (obj *SignedByte) Neg() {
	pos := (obj.Bytes[0] & 0x80) != 0 // neg => pos
	for i, b := range obj.Bytes {
		obj.Bytes[i] = ^b
	}
	carry := true
	for i := len(obj.Bytes) - 1; 0 <= i && carry; i-- {
		b := obj.Bytes[i]
		u := uint16(b) + 1
		obj.Bytes[i] = byte(u & 0x00ff)
		carry = 255 < u
	}
	// If positive and has high bit then prepend 0x00. Going from negative to
	// positive never overflows.
	if pos && (obj.Bytes[0]&0x80) != 0 {
		obj.Bytes = append([]byte{0}, obj.Bytes...)
	}
}
