// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

// RealSymbol is the symbol with a value of "real".
const RealSymbol = Symbol("real")

// Real exists to allow assertions to determine if an Object is an real.
type Real interface {
	Number

	// RealType returns the real type of the instance which can be one of:
	// fixnum, bignum, float, or ratio.
	RealType() Symbol
}
