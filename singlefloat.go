// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import (
	"bytes"
	"math/big"
	"strconv"
)

// SingleFloatSymbol is the symbol with a value of "singleFloat".
const SingleFloatSymbol = Symbol("single-float")

// SingleFloat is a float32 Object.
type SingleFloat float32

// String representation of the Object.
func (obj SingleFloat) String() string {
	return string(obj.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (obj SingleFloat) Append(b []byte) []byte {
	return obj.Readably(b, &printer)
}

// Readably appends the object to a byte slice. If p.Readbly is true the
// objects is appended in a readable format otherwise a simple append which
// may or may not be readable.
func (obj SingleFloat) Readably(b []byte, p *Printer) []byte {
	// Use the LISP exponent nomenclature by forming the buffer and
	// then replacing the 'e'.
	var tmp []byte
	switch {
	case p.Readably:
		// float32 precision is 7.
		if p.Prec < 7 {
			tmp = strconv.AppendFloat([]byte{}, float64(obj), 'e', p.Prec, 32)
		} else {
			tmp = strconv.AppendFloat([]byte{}, float64(obj), 'e', -1, 32)
		}
		b = append(b, bytes.ReplaceAll(bytes.ToLower(tmp), []byte{'e'}, []byte{'s'})...)
	case p.Prec < 7:
		b = strconv.AppendFloat(b, float64(obj), 'g', p.Prec, 32)
	default:
		tmp = strconv.AppendFloat([]byte{}, float64(obj), 'g', -1, 32)
		b = append(b, bytes.ReplaceAll(bytes.ToLower(tmp), []byte{'e'}, []byte{'s'})...)
	}
	return b
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

// LoadForm returns a form that can be evaluated to create the object.
func (obj SingleFloat) LoadForm() Object {
	return obj
}
