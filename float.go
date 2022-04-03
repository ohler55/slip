// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import (
	"math/big"
	"strconv"
)

// FloatSymbol is the symbol with a value of "float".
const FloatSymbol = Symbol("float")

func init() {
	DefConstant(FloatSymbol, FloatSymbol, `A _float_ represents a decimal _number_.`)
}

// Float is a int64 Object.
type Float float64

// String representation of the Object.
func (obj Float) String() string {
	return strconv.FormatFloat(float64(obj), 'g', -1, 64)
}

// Append a buffer with a representation of the Object.
func (obj Float) Append(b []byte) []byte {
	return strconv.AppendFloat(b, float64(obj), 'g', -1, 64)
}

// Simplify the Object into a float64.
func (obj Float) Simplify() interface{} {
	return float64(obj)
}

// Equal returns true if this Object and the other are equal in value.
func (obj Float) Equal(other Object) (eq bool) {
	switch to := other.(type) {
	case Fixnum:
		eq = obj == Float(to)
	case Float:
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
func (obj Float) Hierarchy() []Symbol {
	return []Symbol{FloatSymbol, RealSymbol, NumberSymbol, TrueSymbol}
}

// RealType returns 'float.
func (obj Float) RealType() Symbol {
	return FloatSymbol
}

// NumberType returns 'float.
func (obj Float) NumberType() Symbol {
	return FloatSymbol
}

// Eval returns self.
func (obj Float) Eval(s *Scope, depth int) Object {
	return obj
}
