// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

// ArraySymbol is the symbol with a value of "array".
const ArraySymbol = Symbol("array")

func init() {
	DefConstant(ArraySymbol, ArraySymbol,
		`An _array_ is an _n_ dimensional collection of _objects_ identified by _fixnum_ indices on each dimension.`)
}
