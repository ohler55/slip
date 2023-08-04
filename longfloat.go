// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import (
	"fmt"
	"math/big"
)

// LongFloatSymbol is the symbol with a value of "longFloat".
const (
	LongFloatSymbol = Symbol("long-float")
	// An approximation for converting base 10 to base 2 precision since
	// big.Float uses base 10 precision for formatting and base 2 for parsing.
	prec10t2 = 3.32
)

func init() {
	DefConstant(LongFloatSymbol, LongFloatSymbol,
		`A _long-float_ represents a decimal _number_ or _float_. It is implemented
as a float64 as defined by IEEE 754 as a long precision decimal with 16 significant
digits and a maximum exponent of 308.`)
	// big.MaxExp causes a parse failure. The closest that does is 3 less.
	mostPos, _, _ := big.ParseFloat(fmt.Sprintf("1.0e%d", big.MaxExp-3), 10, 10, big.ToNearestAway)
	mostNeg, _, _ := big.ParseFloat(fmt.Sprintf("-1.0e%d", big.MaxExp-3), 10, 10, big.ToNearestAway)
	DefConstant(Symbol("most-positive-long-float"), (*LongFloat)(mostPos),
		"The most positive value a _long-float_ can have.")
	DefConstant(Symbol("most-negative-long-float"), (*LongFloat)(mostNeg),
		"The most negative value a _long-float_ can have.")

	leastPos, _, _ := big.ParseFloat(fmt.Sprintf("1.0e%d", big.MinExp), 10, 10, big.ToNearestAway)
	leastNeg, _, _ := big.ParseFloat(fmt.Sprintf("-1.0e%d", big.MinExp), 10, 10, big.ToNearestAway)
	DefConstant(Symbol("least-positive-long-float"), (*LongFloat)(leastPos),
		"The smallest non-zero positive value a _long-float_ can have.")
	DefConstant(Symbol("least-negative-long-float"), (*LongFloat)(leastNeg),
		"The smallest non-zero negative value a _long-float_ can have.")
	DefConstant(Symbol("least-positive-normalized-long-float"), (*LongFloat)(leastPos),
		"The smallest non-zero positive value a _long-float_ can have.")
	DefConstant(Symbol("least-negative-normalized-long-float"), (*LongFloat)(leastNeg),
		"The smallest non-zero negative value a _long-float_ can have.")

	epsilon, _, _ := big.ParseFloat(fmt.Sprintf("1.0e%d", big.MinExp), 10, 10, big.ToNearestAway)
	DefConstant(Symbol("long-float-epsilon"), (*LongFloat)(epsilon),
		`The smallest positive _long-float_ such the addition of the epsilon value to
1.0L0 returns a value greater than 1.0L0.`)
	DefConstant(Symbol("long-float-negative-epsilon"), (*LongFloat)(epsilon),
		`The smallest positive _long-float_ such the subtraction of the epsilon value from
1.0L0 returns a value less than 1.0L0.`)
}

// LongFloat is a big.float Object.
type LongFloat big.Float

// NewLongFloat creates a new Float.
func NewLongFloat(num float64) *LongFloat {
	return (*LongFloat)(big.NewFloat(num))
}

// String representation of the Object.
func (obj *LongFloat) String() string {
	return string(obj.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (obj *LongFloat) Append(b []byte) []byte {
	return printer.Append(b, obj, 0)
}

// Simplify the Object into a float64.
func (obj *LongFloat) Simplify() interface{} {
	return string(Append([]byte{}, obj))
}

// Equal returns true if this Object and the other are equal in value.
func (obj *LongFloat) Equal(other Object) (eq bool) {
	switch to := other.(type) {
	case Fixnum:
		eq = (*big.Float)(obj).Cmp(big.NewFloat(float64(to))) == 0
	case SingleFloat:
		eq = (*big.Float)(obj).Cmp(big.NewFloat(float64(to))) == 0
	case DoubleFloat:
		eq = (*big.Float)(obj).Cmp(big.NewFloat(float64(to))) == 0
	case *LongFloat:
		eq = (*big.Float)(obj).Cmp((*big.Float)(to)) == 0
	case *Ratio:
		f, exact := (*big.Rat)(to).Float64()
		eq = exact && (*big.Float)(obj).Cmp(big.NewFloat(f)) == 0
	case *Bignum:
		f := (*big.Float)(obj)
		if f.IsInt() {
			i, _ := f.Int(nil)
			eq = i.Cmp((*big.Int)(to)) == 0
		}
	}
	return
}

// Hierarchy returns the class hierarchy as symbols for the instance.
func (obj *LongFloat) Hierarchy() []Symbol {
	return []Symbol{LongFloatSymbol, FloatSymbol, RealSymbol, NumberSymbol, TrueSymbol}
}

// FloatType returns 'long-float.
func (obj *LongFloat) FloatType() Symbol {
	return LongFloatSymbol
}

// RealType returns 'long-float.
func (obj *LongFloat) RealType() Symbol {
	return LongFloatSymbol
}

// NumberType returns 'long-float.
func (obj *LongFloat) NumberType() Symbol {
	return LongFloatSymbol
}

// Eval returns self.
func (obj *LongFloat) Eval(s *Scope, depth int) Object {
	return obj
}

// RealValue of the number as a float64.
func (obj *LongFloat) RealValue() float64 {
	f, _ := (*big.Float)(obj).Float64()
	return f
}
