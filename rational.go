// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

// RationalSymbol is the symbol with a value of "rational".
const RationalSymbol = Symbol("rational")

// Rational exists to allow assertions to determine if an Object is an rational.
type Rational interface {
	Real

	// RationalType returns the rational type of the instance which can be one
	// of: fixnum, bignum, or ratio.
	RationalType() Symbol
}
