// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import (
	"bytes"
	"math/big"
)

// LongFloatSymbol is the symbol with a value of "longFloat".
const (
	LongFloatSymbol = Symbol("long-float")
	// An approximation for converting base 10 to base 2 precision since
	// big.Float uses base 10 precision for formatting and base 2 for parsing.
	prec10t2 = 3.32
)

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
	return obj.Readably(b, &printer)
}

// Readably appends the object to a byte slice. If p.Readbly is true the
// objects is appended in a readable format otherwise a simple append which
// may or may not be readable.
func (obj *LongFloat) Readably(b []byte, p *Printer) []byte {
	prec := -1
	if 0 < p.Prec {
		prec = int(float64((*big.Float)(obj).Prec()) / prec10t2)
		if p.Prec < prec {
			prec = p.Prec
		}
	}
	if p.Readably {
		// Use the LISP exponent nomenclature by forming the buffer and
		// then replacing the 'e'.
		tmp := (*big.Float)(obj).Append([]byte{}, 'e', prec)
		b = append(b, bytes.ReplaceAll(tmp, []byte{'e'}, []byte{'L'})...)
	} else {
		b = (*big.Float)(obj).Append(b, 'g', prec)
	}
	return b
}

// Simplify the Object into a float64.
func (obj *LongFloat) Simplify() any {
	return string(Append([]byte{}, obj))
}

// Equal returns true if this Object and the other are equal in value.
func (obj *LongFloat) Equal(other Object) (eq bool) {
	switch to := other.(type) {
	case Fixnum:
		eq = (*big.Float)(obj).Cmp(big.NewFloat(float64(to))) == 0
	case Octet:
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

// LoadForm returns a form that can be evaluated to create the object.
func (obj *LongFloat) LoadForm() Object {
	return obj
}
