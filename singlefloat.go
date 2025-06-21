// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import (
	"math"
	"math/big"
)

// SingleFloatSymbol is the symbol with a value of "singleFloat".
const SingleFloatSymbol = Symbol("single-float")

func init() {
	DefConstant(&CLPkg, string(SingleFloatSymbol), SingleFloatSymbol,
		`A _single-float_ represents a decimal _number_ or _float_. It is implemented
as a float32 as defined by IEEE 754 as a single precision decimal with 7 significant
digits and a maximum exponent of 38.`)
	DefConstant(&CLPkg, "most-positive-single-float", SingleFloat(math.MaxFloat32),
		"The most positive value a _single-float_ can have.")
	DefConstant(&CLPkg, "most-negative-single-float", SingleFloat(-math.MaxFloat32),
		"The most negative value a _single-float_ can have.")
	DefConstant(&CLPkg, "least-positive-single-float", SingleFloat(math.SmallestNonzeroFloat32),
		"The smallest non-zero positive value a _single-float_ can have.")
	DefConstant(&CLPkg, "least-negative-single-float", SingleFloat(-math.SmallestNonzeroFloat32),
		"The smallest non-zero negative value a _single-float_ can have.")
	DefConstant(&CLPkg, "least-positive-normalized-single-float", SingleFloat(math.SmallestNonzeroFloat32),
		"The smallest non-zero positive value a _single-float_ can have.")
	DefConstant(&CLPkg, "least-negative-normalized-single-float", SingleFloat(-math.SmallestNonzeroFloat32),
		"The smallest non-zero negative value a _single-float_ can have.")
	DefConstant(&CLPkg, "single-float-epsilon", SingleFloat(5.960465e-8),
		`The smallest positive _single-float_ such the addition of the epsilon value to
1.0s0 returns a value greater than 1.0s0.`)
	DefConstant(&CLPkg, "single-float-negative-epsilon", SingleFloat(5.960465e-8),
		`The smallest positive _single-float_ such the subtraction of the epsilon value from
1.0s0 returns a value less than 1.0s0.`)
}

// SingleFloat is a float32 Object.
type SingleFloat float32

// String representation of the Object.
func (obj SingleFloat) String() string {
	return string(obj.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (obj SingleFloat) Append(b []byte) []byte {
	return printer.Append(b, obj, 0)
}

// Simplify the Object into a float64.
func (obj SingleFloat) Simplify() any {
	return float64(obj)
}

// Equal returns true if this Object and the other are equal in value.
func (obj SingleFloat) Equal(other Object) (eq bool) {
	switch to := other.(type) {
	case Fixnum:
		eq = obj == SingleFloat(to)
	case Octet:
		eq = obj == SingleFloat(to)
	case SingleFloat:
		eq = obj == to
	case DoubleFloat:
		eq = float64(obj) == float64(to)
	case *LongFloat:
		eq = big.NewFloat(float64(obj)).Cmp((*big.Float)(to)) == 0
	case *Ratio:
		f, exact := (*big.Rat)(to).Float64()
		eq = exact && f == float64(obj)
	case *Bignum:
		f := big.NewFloat(float64(obj))
		if f.IsInt() {
			i, _ := f.Int(nil)
			eq = i.Cmp((*big.Int)(to)) == 0
		}
	}
	return
}

// Hierarchy returns the class hierarchy as symbols for the instance.
func (obj SingleFloat) Hierarchy() []Symbol {
	return []Symbol{SingleFloatSymbol, FloatSymbol, RealSymbol, NumberSymbol, TrueSymbol}
}

// FloatType returns 'single-float.
func (obj SingleFloat) FloatType() Symbol {
	return SingleFloatSymbol
}

// RealType returns 'single-float.
func (obj SingleFloat) RealType() Symbol {
	return SingleFloatSymbol
}

// NumberType returns 'single-float.
func (obj SingleFloat) NumberType() Symbol {
	return SingleFloatSymbol
}

// Eval returns self.
func (obj SingleFloat) Eval(s *Scope, depth int) Object {
	return obj
}

// RealValue of the number as a float64.
func (obj SingleFloat) RealValue() float64 {
	return float64(obj)
}
