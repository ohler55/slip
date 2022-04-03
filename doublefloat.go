// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import (
	"math"
	"math/big"
)

// DoubleFloatSymbol is the symbol with a value of "doubleFloat".
const DoubleFloatSymbol = Symbol("double-float")

func init() {
	DefConstant(DoubleFloatSymbol, DoubleFloatSymbol,
		`A _double-float_ represents a decimal _number_ or _float_. It is implemented
as a float64 as defined by IEEE 754 as a double precision decimal with 16 significant
digits and a maximum exponent of 308.`)
	DefConstant(Symbol("most-positive-double-float"), DoubleFloat(math.MaxFloat64),
		"The most positive value a _double-float_ can have.")
	DefConstant(Symbol("most-negative-double-float"), DoubleFloat(-math.MaxFloat64),
		"The most negative value a _double-float_ can have.")
	DefConstant(Symbol("least-positive-double-float"), DoubleFloat(math.SmallestNonzeroFloat64),
		"The smallest non-zero positive value a _double-float_ can have.")
	DefConstant(Symbol("least-negative-double-float"), DoubleFloat(-math.SmallestNonzeroFloat64),
		"The smallest non-zero negative value a _double-float_ can have.")
	DefConstant(Symbol("double-float-epsilon"), DoubleFloat(1.1102230246251568e-16),
		`The smallest positive _double-float_ such the addition of the epsilon value to
1.0d0 returns a value greater than 1.0d0.`)
	DefConstant(Symbol("double-float-negative-epsilon"), DoubleFloat(1.1102230246251568e-16),
		`The smallest positive _double-float_ such the subtraction of the epsilon value from
1.0d0 returns a value less than 1.0d0.`)
}

// DoubleFloat is a float64 Object.
type DoubleFloat float64

// String representation of the Object.
func (obj DoubleFloat) String() string {
	return string(obj.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (obj DoubleFloat) Append(b []byte) []byte {
	return printer.Append(b, obj, 0)
}

// Simplify the Object into a float64.
func (obj DoubleFloat) Simplify() interface{} {
	return float64(obj)
}

// Equal returns true if this Object and the other are equal in value.
func (obj DoubleFloat) Equal(other Object) (eq bool) {
	switch to := other.(type) {
	case Fixnum:
		eq = obj == DoubleFloat(to)
	case SingleFloat:
		eq = float64(obj) == float64(to)
	case DoubleFloat:
		eq = obj == to
	case *Ratio:
		f, exact := (*big.Rat)(to).Float64()
		eq = exact && f == float64(obj)
	case *Bignum:
		f := big.NewFloat(float64(obj))
		if f.IsInt() {
			i, _ := f.Int(nil)
			eq = i.Cmp((*big.Int)(to)) == 0
		}

		// TBD Complex
	}
	return
}

// Hierarchy returns the class hierarchy as symbols for the instance.
func (obj DoubleFloat) Hierarchy() []Symbol {
	return []Symbol{DoubleFloatSymbol, FloatSymbol, RealSymbol, NumberSymbol, TrueSymbol}
}

// FloatType returns 'double-float.
func (obj DoubleFloat) FloatType() Symbol {
	return DoubleFloatSymbol
}

// RealType returns 'double-float.
func (obj DoubleFloat) RealType() Symbol {
	return DoubleFloatSymbol
}

// NumberType returns 'double-float.
func (obj DoubleFloat) NumberType() Symbol {
	return DoubleFloatSymbol
}

// Eval returns self.
func (obj DoubleFloat) Eval(s *Scope, depth int) Object {
	return obj
}
