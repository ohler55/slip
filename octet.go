// Copyright (c) 2024, Peter Ohler, All rights reserved.

package slip

import (
	"math/big"
)

// OctetSymbol is the symbol with a value of "octet".
const OctetSymbol = Symbol("octet")

// Octet is a unsigned 8 bit integer.
type Octet byte

// String representation of the Object.
func (obj Octet) String() string {
	return string(obj.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (obj Octet) Append(b []byte) []byte {
	return printer.Append(b, obj, 0)
}

// Simplify the Object into an int64.
func (obj Octet) Simplify() any {
	return int64(obj)
}

// Equal returns true if this Object and the other are equal in value.
func (obj Octet) Equal(other Object) (eq bool) {
	switch to := other.(type) {
	case Octet:
		eq = obj == to
	case Fixnum:
		eq = Fixnum(obj) == to
	case SingleFloat:
		eq = SingleFloat(obj) == to
	case DoubleFloat:
		eq = DoubleFloat(obj) == to
	case *LongFloat:
		eq = big.NewFloat(float64(obj)).Cmp((*big.Float)(to)) == 0
	case *Ratio:
		rat := (*big.Rat)(to)
		eq = rat.IsInt() && rat.Num().IsInt64() && rat.Num().Int64() == int64(obj)
	case *Bignum:
		num := (*big.Int)(to)
		eq = num.IsInt64() && num.Int64() == int64(obj)
	}
	return
}

// Hierarchy returns the class hierarchy as symbols for the instance.
func (obj Octet) Hierarchy() []Symbol {
	return []Symbol{OctetSymbol, IntegerSymbol, RationalSymbol, RealSymbol, NumberSymbol, TrueSymbol}
}

// IntegerType returns 'octet.
func (obj Octet) IntegerType() Symbol {
	return OctetSymbol
}

// RationalType returns 'octet.
func (obj Octet) RationalType() Symbol {
	return OctetSymbol
}

// RealType returns 'octet.
func (obj Octet) RealType() Symbol {
	return OctetSymbol
}

// NumberType returns 'octet.
func (obj Octet) NumberType() Symbol {
	return OctetSymbol
}

// Eval returns self.
func (obj Octet) Eval(s *Scope, depth int) Object {
	return obj
}

// RealValue of the number as a float64.
func (obj Octet) RealValue() float64 {
	return float64(obj)
}

// IsInt64 returns true if the instance can be represented by an int64.
func (obj Octet) IsInt64() bool {
	return true
}

// Int64 of the number.
func (obj Octet) Int64() int64 {
	return int64(obj)
}
