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

// SignedByte represents an integer with a specific number of bits.
type SignedByte struct {
	Bytes []byte // low bits at the end, big-endian unsigned
	Size  uint
	Neg   bool
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
		return obj.Size == sb.Size && obj.Neg == sb.Neg && bytes.Equal(obj.Bytes, sb.Bytes)
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
	if obj.Size <= 64 {
		var i64 int64
		for _, b := range obj.Bytes {
			i64 = i64<<8 | int64(b)
		}
		if obj.Neg {
			i64 = -i64
		}
		return Fixnum(i64)
	}
	var bi big.Int
	_ = bi.SetBytes(obj.Bytes)
	if obj.Neg {
		_ = bi.Neg(&bi)
	}
	return (*Bignum)(&bi)
}

// TBD operations on the bytes or leave those for the various functions?
