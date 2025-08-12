// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import (
	"math/big"
)

// DoubleFloatSymbol is the symbol with a value of "doubleFloat".
const DoubleFloatSymbol = Symbol("double-float")

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
func (obj DoubleFloat) Simplify() any {
	return float64(obj)
}

// Equal returns true if this Object and the other are equal in value.
func (obj DoubleFloat) Equal(other Object) (eq bool) {
	switch to := other.(type) {
	case Fixnum:
		eq = obj == DoubleFloat(to)
	case Octet:
		eq = obj == DoubleFloat(to)
	case SingleFloat:
		eq = float64(obj) == float64(to)
	case DoubleFloat:
		eq = obj == to
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

// RealValue of the number as a float64.
func (obj DoubleFloat) RealValue() float64 {
	return float64(obj)
}

// LoadForm returns a form that can be evaluated to create the object or nil
// if that is not possible.
func (obj DoubleFloat) LoadForm() Object {
	return obj
}
