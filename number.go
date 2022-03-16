// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

// NumberSymbol is the symbol with a value of "number".
const NumberSymbol = Symbol("number")

func init() {
	DefConstant(NumberSymbol, NumberSymbol, `A _number_ is a combination of the _real_ and _complex_ numbers.`)
}

// Number exists to allow assertions to determine if an Object is an number.
type Number interface {
	Object

	// NumberType returns the number type of the instance which can be one of:
	// fixnum, bignum, float, ratio, or complex.
	NumberType() Symbol
}
