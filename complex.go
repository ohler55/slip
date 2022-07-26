// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import (
	"fmt"
	"strconv"
)

// ComplexSymbol is the symbol with a value of "complex".
const ComplexSymbol = Symbol("complex")

func init() {
	DefConstant(ComplexSymbol, ComplexSymbol,
		`A _complex_ is a _number_ composed of a real and imaginary part.`)
}

// Complex is a numerator and denominator pair.
type Complex complex128

// String representation of the Object.
func (obj Complex) String() string {
	return string(obj.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (obj Complex) Append(b []byte) []byte {
	b = append(b, "#C("...)
	b = printer.Append(b, Fixnum(real(obj)), 0)
	b = append(b, ' ')
	b = printer.Append(b, Fixnum(imag(obj)), 0)

	return append(b, ')')
}

// Simplify the Object into an int64.
func (obj Complex) Simplify() interface{} {
	var b []byte
	b = strconv.AppendInt(b, int64(real(obj)), 10)
	i := int64(imag(obj))
	if 0 <= i {
		b = append(b, '+')
	}
	b = strconv.AppendInt(b, i, 10)
	b = append(b, 'i')

	return string(b)
}

// Equal returns true if this Object and the other are equal in value.
func (obj Complex) Equal(other Object) (eq bool) {
	if to, ok := other.(Complex); ok && complex128(obj) == complex128(to) {
		eq = true
	}
	return
}

// Hierarchy returns the class hierarchy as symbols for the instance.
func (obj Complex) Hierarchy() []Symbol {
	return []Symbol{ComplexSymbol, NumberSymbol, TrueSymbol}
}

// NumberType returns 'complex.
func (obj Complex) NumberType() Symbol {
	return ComplexSymbol
}

// Eval returns self.
func (obj Complex) Eval(s *Scope, depth int) Object {
	return obj
}

func newComplex(list List) Complex {
	if len(list) != 2 {
		panic(fmt.Sprintf("Can not forms a complex object from %s.", list))
	}
	var (
		r float64
		i float64
	)
	if rv, ok := list[1].(Real); ok {
		r = rv.RealValue()
	} else {
		panic(fmt.Sprintf("Can not convert %s, a %T into a float.", list[1], list[1]))
	}
	if rv, ok := list[0].(Real); ok {
		i = rv.RealValue()
	} else {
		panic(fmt.Sprintf("Can not convert %s, a %T into a float.", list[0], list[0]))
	}
	return Complex(complex(r, i))
}
