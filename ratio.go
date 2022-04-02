// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

// RatioSymbol is the symbol with a value of "ratio".
const RatioSymbol = Symbol("ratio")

func init() {
	DefConstant(RatioSymbol, RatioSymbol,
		`A _ratio_ is a _number_ represented as a ratio of two integers.`)
}

// Ratio is a numerator and denominator pair.
type Ratio struct {
	Numerator   int64
	Denominator uint64
}

// NewRatio creates a new Ratio.
func NewRatio(num int64, denom uint64) *Ratio {
	r := Ratio{
		Numerator:   num,
		Denominator: denom,
	}
	r.Reduce()

	return &r
}

// String representation of the Object.
func (obj *Ratio) String() string {
	return string(obj.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (obj *Ratio) Append(b []byte) []byte {
	return printer.Append(b, obj, 0)
}

// Simplify the Object into an int64.
func (obj *Ratio) Simplify() interface{} {
	return float64(obj.Numerator) / float64(obj.Denominator)
}

// Equal returns true if this Object and the other are equal in value.
func (obj *Ratio) Equal(other Object) (eq bool) {
	switch to := other.(type) {
	case Fixnum:
		eq = obj.Denominator == 1 && int64(to) == obj.Numerator
	case Float:
		eq = float64(obj.Numerator)/float64(obj.Denominator) == float64(to)
	case *Ratio:
		eq = obj.Denominator == to.Denominator && obj.Numerator == to.Numerator

		// TBD Complex
		// TBD Bignum
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

// Reduce the Numerator and Denominator until the common divisor is one.
func (obj *Ratio) Reduce() {
	if obj.Denominator == 0 {
		panic("The denominator of a ratio must be greater than zero")
	}
	if obj.Numerator == 0 {
		obj.Denominator = 1
		return
	}
	var a uint64
	if obj.Numerator < 0 {
		a = uint64(-obj.Numerator)
	} else {
		a = uint64(obj.Numerator)
	}
	b := obj.Denominator
	if a < b {
		a, b = b, a
	}
	for {
		r := a % b
		if r == 0 {
			break
		}
		a = b
		b = r
	}
	obj.Numerator /= int64(b)
	obj.Denominator /= b
}
