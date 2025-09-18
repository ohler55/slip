// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import (
	"bytes"
	"math/big"
	"strconv"
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
	return obj.Readably(b, &printer)
}

// Readably appends the object to a byte slice. If p.Readbly is true the
// objects is appended in a readable format otherwise a simple append which
// may or may not be readable.
func (obj DoubleFloat) Readably(b []byte, p *Printer) []byte {
	// Use the LISP exponent nomenclature by forming the buffer and
	// then replacing the 'e'.
	var tmp []byte
	switch {
	case p.Readably:
		// float64 precision is 16.
		if p.Prec < 16 {
			tmp = strconv.AppendFloat([]byte{}, float64(obj), 'e', p.Prec, 64)
		} else {
			tmp = strconv.AppendFloat([]byte{}, float64(obj), 'e', -1, 64)
		}
		b = append(b, bytes.ReplaceAll(bytes.ToLower(tmp), []byte{'e'}, []byte{'d'})...)
	case p.Prec < 16:
		b = strconv.AppendFloat(b, float64(obj), 'g', p.Prec, 64)
	default:
		tmp = strconv.AppendFloat([]byte{}, float64(obj), 'g', -1, 64)
		b = append(b, bytes.ReplaceAll(bytes.ToLower(tmp), []byte{'e'}, []byte{'d'})...)
	}
	return b
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

// LoadForm returns a form that can be evaluated to create the object.
func (obj DoubleFloat) LoadForm() Object {
	return obj
}
