// Copyright (c) 2025, Peter Ohler, All rights reserved.

package slip

// BitSymbol is the symbol with a value of "bit".
const BitSymbol = Symbol("bit")

func init() {
	DefConstant(BitSymbol, BitSymbol, `A _bit_ is a one bit integer.`)
}

// Bit represents a one bit integer.
type Bit byte

// String representation of the Object.
func (obj Bit) String() string {
	return string(obj.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (obj Bit) Append(b []byte) []byte {
	if obj == 0 {
		return append(b, '0')
	}
	return append(b, '1')
}

// Simplify the Object into an int64.
func (obj Bit) Simplify() any {
	return Fixnum(obj).Simplify()
}

// Equal returns true if this Object and the other are equal in value.
func (obj Bit) Equal(other Object) (eq bool) {
	return Fixnum(obj).Equal(other)
}

// Hierarchy returns the class hierarchy as symbols for the instance.
func (obj Bit) Hierarchy() []Symbol {
	return []Symbol{
		BitSymbol,
		UnsignedByteSymbol,
		SignedByteSymbol,
		IntegerSymbol,
		RationalSymbol,
		RealSymbol,
		NumberSymbol,
		TrueSymbol,
	}
}

// IntegerType returns 'bit.
func (obj Bit) IntegerType() Symbol {
	return BitSymbol
}

// RationalType returns 'bit.
func (obj Bit) RationalType() Symbol {
	return BitSymbol
}

// RealType returns 'bit.
func (obj Bit) RealType() Symbol {
	return BitSymbol
}

// NumberType returns 'bit.
func (obj Bit) NumberType() Symbol {
	return BitSymbol
}

// Eval returns self.
func (obj Bit) Eval(s *Scope, depth int) Object {
	return obj
}

// RealValue of the number as a float64.
func (obj Bit) RealValue() float64 {
	return float64(obj)
}

// IsInt64 returns true if the instance can be represented by an int64.
func (obj Bit) IsInt64() bool {
	return true
}

// Int64 of the number.
func (obj Bit) Int64() (i int64) {
	return int64(obj)
}
