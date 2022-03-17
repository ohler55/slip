// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import (
	"math"
	"strconv"
)

// FixnumSymbol is the symbol with a value of "fixnum".
const FixnumSymbol = Symbol("fixnum")

func init() {
	DefConstant(FixnumSymbol, FixnumSymbol,
		`A _fixnum_ is an _integer_ in the range from _most-negative-fixnum_ and _most-positive-fixnum_ inclusive.`)
	DefConstant(Symbol("most-positive-fixnum"), Fixnum(math.MaxInt64),
		"The most positive value a _fixnum_ can have.")
	DefConstant(Symbol("most-negative-fixnum"), Fixnum(math.MinInt64),
		"The most negative value a _fixnum_ can have.")
}

// Fixnum is a int64 Object.
type Fixnum int64

// String representation of the Object.
func (obj Fixnum) String() string {
	return strconv.FormatInt(int64(obj), 10)
}

// Append a buffer with a representation of the Object.
func (obj Fixnum) Append(b []byte) []byte {
	return strconv.AppendInt(b, int64(obj), 10)
}

// Simplify the Object into an int64.
func (obj Fixnum) Simplify() interface{} {
	return int64(obj)
}

// Equal returns true if this Object and the other are equal in value.
func (obj Fixnum) Equal(other Object) (eq bool) {
	switch to := other.(type) {
	case Fixnum:
		eq = obj == to
	case Float:
		eq = Float(obj) == to
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
