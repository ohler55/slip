// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import "math/big"

// BignumSymbol is the symbol with a value of "bignum".
const BignumSymbol = Symbol("bignum")

func init() {
	DefConstant(BignumSymbol, BignumSymbol,
		`A _bignum_ is a _number_ represented as a bignum of two integers.`)
}

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
func (obj *Bignum) Simplify() interface{} {
	if (*big.Int)(obj).IsInt64() {
		return (*big.Int)(obj).Int64()
	}
	return string(printer.Append([]byte{}, obj, 0))
}

// Equal returns true if this Object and the other are equal in value.
func (obj *Bignum) Equal(other Object) (eq bool) {
	switch to := other.(type) {
	case Fixnum:
		num := (*big.Int)(obj)
		eq = num.IsInt64() && num.Int64() == int64(to)
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
	case *Ratio:
		rat := (*big.Rat)(to)
		eq = rat.IsInt() && (*big.Int)(obj).Cmp(rat.Num()) == 0
	case *Bignum:
		eq = (*big.Int)(obj).Cmp((*big.Int)(to)) == 0

		// TBD Complex
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
