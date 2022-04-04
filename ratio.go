// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import "math/big"

// RatioSymbol is the symbol with a value of "ratio".
const RatioSymbol = Symbol("ratio")

func init() {
	DefConstant(RatioSymbol, RatioSymbol,
		`A _ratio_ is a _number_ represented as a ratio of two integers.`)
}

// Ratio is a numerator and denominator pair.
type Ratio big.Rat

// NewRatio creates a new Ratio.
func NewRatio(num, denom int64) *Ratio {
	return (*Ratio)(big.NewRat(num, denom))
}

// String representation of the Object.
func (obj *Ratio) String() string {
	return string(obj.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (obj *Ratio) Append(b []byte) []byte {
	return printer.Append(b, obj, 0)
}

// Simplify the Object into an int64.
func (obj *Ratio) Simplify() interface{} {
	f, exact := (*big.Rat)(obj).Float64()
	if exact {
		return f
	}
	return string(printer.Append([]byte{}, obj, 0))
}

// Equal returns true if this Object and the other are equal in value.
func (obj *Ratio) Equal(other Object) (eq bool) {
	switch to := other.(type) {
	case Fixnum:
		rat := (*big.Rat)(obj)
		eq = rat.IsInt() && rat.Num().IsInt64() && rat.Num().Int64() == int64(to)
	case SingleFloat:
		f, exact := (*big.Rat)(obj).Float64()
		eq = exact && f == float64(to)
	case DoubleFloat:
		f, exact := (*big.Rat)(obj).Float64()
		eq = exact && f == float64(to)
	case *LongFloat:
		f, exact := (*big.Rat)(obj).Float64()
		f2, accuracy := (*big.Float)(to).Float64()
		eq = exact && accuracy == big.Exact && f == f2
	case *Ratio:
		eq = (*big.Rat)(obj).Cmp((*big.Rat)(to)) == 0
	case *Bignum:
		rat := (*big.Rat)(obj)
		eq = rat.IsInt() && (*big.Int)(to).Cmp(rat.Num()) == 0

		// TBD Complex
	}
	return
}

// Hierarchy returns the class hierarchy as symbols for the instance.
func (obj *Ratio) Hierarchy() []Symbol {
	return []Symbol{RatioSymbol, RationalSymbol, RealSymbol, NumberSymbol, TrueSymbol}
}

// RationalType returns 'ratio.
func (obj *Ratio) RationalType() Symbol {
	return RatioSymbol
}

// RealType returns 'ratio.
func (obj *Ratio) RealType() Symbol {
	return RatioSymbol
}

// NumberType returns 'ratio.
func (obj *Ratio) NumberType() Symbol {
	return RatioSymbol
}

// Eval returns self.
func (obj *Ratio) Eval(s *Scope, depth int) Object {
	return obj
}
