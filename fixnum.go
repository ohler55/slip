// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import (
	"math/big"
	"strconv"
)

// FixnumSymbol is the symbol with a value of "fixnum".
const FixnumSymbol = Symbol("fixnum")

// Fixnum is a int64 Object.
type Fixnum int64

// String representation of the Object.
func (obj Fixnum) String() string {
	return string(obj.Readably(nil, &printer))
}

// Append a buffer with a representation of the Object.
func (obj Fixnum) Append(b []byte) []byte {
	return obj.Readably(b, &printer)
}

// Readably appends the object to a byte slice. If p.Readbly is true the
// objects is appended in a readable format otherwise a simple append which
// may or may not be readable.
func (obj Fixnum) Readably(b []byte, p *Printer) []byte {
	if p.Radix {
		switch p.Base {
		case 2:
			b = append(b, "#b"...)
			b = strconv.AppendInt(b, int64(obj), int(p.Base))
		case 8:
			b = append(b, "#o"...)
			b = strconv.AppendInt(b, int64(obj), int(p.Base))
		case 16:
			b = append(b, "#x"...)
			b = strconv.AppendInt(b, int64(obj), int(p.Base))
		case 10:
			b = strconv.AppendInt(b, int64(obj), 10)
			b = append(b, '.')
		default:
			b = append(b, '#')
			b = strconv.AppendInt(b, int64(p.Base), 10)
			b = append(b, 'r')
			b = strconv.AppendInt(b, int64(obj), int(p.Base))
		}
	} else {
		b = strconv.AppendInt(b, int64(obj), int(p.Base))
	}
	return b
}

// Simplify the Object into an int64.
func (obj Fixnum) Simplify() any {
	return int64(obj)
}

// Equal returns true if this Object and the other are equal in value.
func (obj Fixnum) Equal(other Object) (eq bool) {
	switch to := other.(type) {
	case Fixnum:
		eq = obj == to
	case *Bignum:
		num := (*big.Int)(to)
		eq = num.IsInt64() && num.Int64() == int64(obj)
	case Integer:
		eq = to.IsInt64() && int64(obj) == to.Int64()
	case SingleFloat:
		eq = SingleFloat(obj) == to
	case DoubleFloat:
		eq = DoubleFloat(obj) == to
	case *LongFloat:
		eq = big.NewFloat(float64(obj)).Cmp((*big.Float)(to)) == 0
	case *Ratio:
		rat := (*big.Rat)(to)
		eq = rat.IsInt() && rat.Num().IsInt64() && rat.Num().Int64() == int64(obj)
	}
	return
}

// Hierarchy returns the class hierarchy as symbols for the instance.
func (obj Fixnum) Hierarchy() []Symbol {
	return []Symbol{FixnumSymbol, IntegerSymbol, RationalSymbol, RealSymbol, NumberSymbol, TrueSymbol}
}

// IntegerType returns 'fixnum.
func (obj Fixnum) IntegerType() Symbol {
	return FixnumSymbol
}

// RationalType returns 'fixnum.
func (obj Fixnum) RationalType() Symbol {
	return FixnumSymbol
}

// RealType returns 'fixnum.
func (obj Fixnum) RealType() Symbol {
	return FixnumSymbol
}

// NumberType returns 'fixnum.
func (obj Fixnum) NumberType() Symbol {
	return FixnumSymbol
}

// Eval returns self.
func (obj Fixnum) Eval(s *Scope, depth int) Object {
	return obj
}

// RealValue of the number as a float64.
func (obj Fixnum) RealValue() float64 {
	return float64(obj)
}

// IsInt64 returns true if the instance can be represented by an int64.
func (obj Fixnum) IsInt64() bool {
	return true
}

// Int64 of the number.
func (obj Fixnum) Int64() int64 {
	return int64(obj)
}

// LoadForm returns a form that can be evaluated to create the object.
func (obj Fixnum) LoadForm() Object {
	return obj
}
