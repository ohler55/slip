// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import (
	"math"
	"math/big"
)

// FixnumSymbol is the symbol with a value of "fixnum".
const FixnumSymbol = Symbol("fixnum")

func init() {
	DefConstant(FixnumSymbol, FixnumSymbol,
		`A _fixnum_ is an _integer_ in the range from _most-negative-fixnum_ and _most-positive-fixnum_ inclusive.`)
	DefConstant(Symbol("most-positive-fixnum"), Fixnum(math.MaxInt64),
		"The most positive value a _fixnum_ can have.")
	DefConstant(Symbol("most-negative-fixnum"), Fixnum(math.MinInt64),
		"The most negative value a _fixnum_ can have.")
}

// Fixnum is a int64 Object.
type Fixnum int64

// String representation of the Object.
func (obj Fixnum) String() string {
	return string(obj.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (obj Fixnum) Append(b []byte) []byte {
	return printer.Append(b, obj, 0)
}

// Simplify the Object into an int64.
func (obj Fixnum) Simplify() interface{} {
	return int64(obj)
}

// Equal returns true if this Object and the other are equal in value.
func (obj Fixnum) Equal(other Object) (eq bool) {
	switch to := other.(type) {
	case Fixnum:
		eq = obj == to
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
func (obj Fixnum) Hierarchy() []Symbol {
	return []Symbol{FixnumSymbol, IntegerSymbol, RationalSymbol, RealSymbol, NumberSymbol, TrueSymbol}
}

// IntegerType returns 'fixnum.
func (obj Fixnum) IntegerType() Symbol {
	return FixnumSymbol
}

// RationalType returns 'fixnum.
func (obj Fixnum) RationalType() Symbol {
	return FixnumSymbol
}

// RealType returns 'fixnum.
func (obj Fixnum) RealType() Symbol {
	return FixnumSymbol
}

// NumberType returns 'fixnum.
func (obj Fixnum) NumberType() Symbol {
	return FixnumSymbol
}

// Eval returns self.
func (obj Fixnum) Eval(s *Scope, depth int) Object {
	return obj
}
