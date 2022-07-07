// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

// RealSymbol is the symbol with a value of "real".
const RealSymbol = Symbol("real")

func init() {
	DefConstant(RealSymbol, RealSymbol, `A _real_ is any _number_ that denotes a quantity.`)
}

// Real exists to allow assertions to determine if an Object is an real.
type Real interface {
	Number

	// RealType returns the real type of the instance which can be one of:
	// fixnum, bignum, float, or ratio.
	RealType() Symbol

	// RealValue of the number as a float64.
	RealValue() float64
}
