// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

// FloatSymbol is the symbol with a value of "float".
const FloatSymbol = Symbol("float")

// Float exists to allow assertions to determine if an Object is an float.
type Float interface {
	Real

	// FloatType returns the float type of the instance which can be one of: fixnum or bignum.
	FloatType() Symbol
}
