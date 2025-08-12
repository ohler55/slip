// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import "math/big"

// BignumSymbol is the symbol with a value of "bignum".
const BignumSymbol = Symbol("bignum")

// Bignum is a numerator and denominator pair.
type Bignum big.Int

// NewBignum creates a new Bignum.
func NewBignum(num int64) *Bignum {
	return (*Bignum)(big.NewInt(num))
}

// String representation of the Object.
func (obj *Bignum) String() string {
	return string(obj.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (obj *Bignum) Append(b []byte) []byte {
	return printer.Append(b, obj, 0)
}

// Simplify the Object into an int64.
func (obj *Bignum) Simplify() any {
	if (*big.Int)(obj).IsInt64() {
		return (*big.Int)(obj).Int64()
	}
	return string(printer.Append([]byte{}, obj, 0))
}

// Equal returns true if this Object and the other are equal in value.
func (obj *Bignum) Equal(other Object) (eq bool) {
	switch to := other.(type) {
	case *Bignum:
		eq = (*big.Int)(obj).Cmp((*big.Int)(to)) == 0
	case Integer:
		eq = obj.IsInt64() && to.IsInt64() && obj.Int64() == to.Int64()
	case SingleFloat:
		f := big.NewFloat(float64(to))
		if f.IsInt() {
			i, _ := f.Int(nil)
			eq = i.Cmp((*big.Int)(obj)) == 0
		}
	case DoubleFloat:
		f := big.NewFloat(float64(to))
		if f.IsInt() {
			i, _ := f.Int(nil)
			eq = i.Cmp((*big.Int)(obj)) == 0
		}
	case *LongFloat:
		if (*big.Float)(to).IsInt() {
			i, _ := (*big.Float)(to).Int(nil)
			eq = (*big.Int)(obj).Cmp(i) == 0
		}
	case *Ratio:
		rat := (*big.Rat)(to)
		eq = rat.IsInt() && (*big.Int)(obj).Cmp(rat.Num()) == 0
	}
	return
}

// Hierarchy returns the class hierarchy as symbols for the instance.
func (obj *Bignum) Hierarchy() []Symbol {
	return []Symbol{BignumSymbol, IntegerSymbol, RationalSymbol, RealSymbol, NumberSymbol, TrueSymbol}
}

// IntegerType returns 'fixnum.
func (obj *Bignum) IntegerType() Symbol {
	return BignumSymbol
}

// RationalType returns 'bignum.
func (obj *Bignum) RationalType() Symbol {
	return BignumSymbol
}

// RealType returns 'bignum.
func (obj *Bignum) RealType() Symbol {
	return BignumSymbol
}

// NumberType returns 'bignum.
func (obj *Bignum) NumberType() Symbol {
	return BignumSymbol
}

// Eval returns self.
func (obj *Bignum) Eval(s *Scope, depth int) Object {
	return obj
}

// RealValue of the number as a float64.
func (obj *Bignum) RealValue() float64 {
	f, _ := big.NewFloat(0.0).SetInt((*big.Int)(obj)).Float64()
	return f
}

// IsInt64 returns true if the instance can be represented by an int64.
func (obj *Bignum) IsInt64() bool {
	return (*big.Int)(obj).IsInt64()
}

// Int64 of the number.
func (obj *Bignum) Int64() int64 {
	return (*big.Int)(obj).Int64()
}

// LoadForm returns a form that can be evaluated to create the object or nil
// if that is not possible.
func (obj *Bignum) LoadForm() Object {
	return obj
}
