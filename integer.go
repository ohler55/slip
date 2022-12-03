// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

// IntegerSymbol is the symbol with a value of "integer".
const IntegerSymbol = Symbol("integer")

func init() {
	DefConstant(IntegerSymbol, IntegerSymbol, `An _integer_ is any whole _number_.`)
}

// Integer exists to allow assertions to determine if an Object is an integer.
type Integer interface {
	Rational

	// IntegerType returns the integer type of the instance which can be one of: fixnum or bignum.
	IntegerType() Symbol
}
