// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import (
	"math/big"
	"strconv"
)

// RatioSymbol is the symbol with a value of "ratio".
const RatioSymbol = Symbol("ratio")

// Ratio is a numerator and denominator pair.
type Ratio big.Rat

// NewRatio creates a new Ratio.
func NewRatio(num, denom int64) *Ratio {
	if denom == 0 {
		ArithmeticPanic(NewScope(), 0, Symbol("/"), List{Fixnum(num), Fixnum(denom)}, "division by zero")
	}
	return (*Ratio)(big.NewRat(num, denom))
}

// NewBigRatio creates a new Ratio.
func NewBigRatio(num, denom *big.Int) *Ratio {
	if denom.Sign() == 0 {
		ArithmeticPanic(NewScope(), 0, Symbol("/"), List{(*Bignum)(num), (*Bignum)(denom)}, "division by zero")
	}
	var rat big.Rat
	return (*Ratio)(rat.SetFrac(num, denom))
}

// String representation of the Object.
func (obj *Ratio) String() string {
	return string(obj.Readably(nil, &printer))
}

// Append a buffer with a representation of the Object.
func (obj *Ratio) Append(b []byte) []byte {
	return obj.Readably(b, &printer)
}

// Readably appends the object to a byte slice. If p.Readbly is true the
// objects is appended in a readable format otherwise a simple append which
// may or may not be readable.
func (obj *Ratio) Readably(b []byte, p *Printer) []byte {
	if (*big.Rat)(obj).IsInt() {
		return (*Bignum)((*big.Rat)(obj).Num()).Readably(b, p)
	}
	if p.Radix {
		switch p.Base {
		case 2:
			b = append(b, "#b"...)
		case 8:
			b = append(b, "#o"...)
		case 16:
			b = append(b, "#x"...)
		default:
			b = append(b, '#')
			b = strconv.AppendInt(b, int64(p.Base), 10)
			b = append(b, 'r')
		}
	}
	b = (*big.Rat)(obj).Num().Append(b, int(p.Base))
	b = append(b, '/')
	b = (*big.Rat)(obj).Denom().Append(b, int(p.Base))

	return b
}

// Simplify the Object into an int64.
func (obj *Ratio) Simplify() any {
	if f, exact := (*big.Rat)(obj).Float64(); exact {
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

// RealValue of the number as a float64.
func (obj *Ratio) RealValue() float64 {
	f, _ := (*big.Rat)(obj).Float64()
	return f
}

// LoadForm returns a form that can be evaluated to create the object.
func (obj *Ratio) LoadForm() Object {
	return obj
}
