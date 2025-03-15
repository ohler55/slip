// Copyright (c) 2025, Peter Ohler, All rights reserved.

package slip

// ByteSymbol is the symbol with a value of "byte".
const ByteSymbol = Symbol("byte")

func init() {
	DefConstant(ByteSymbol, ByteSymbol, `A _byte_ represents an 8 bit unsigned integer.`)
}

// Byte is a unisgned 8 bit integer..
type Byte = Octet
