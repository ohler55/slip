// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

// ParseErrorSymbol is the symbol with a value of "parse-error".
const ParseErrorSymbol = Symbol("parse-error")

// ParseError is the interface for all parse-errors.
type ParseError interface {
	Error

	// IsParseError need not do anything other than exist.
	IsParseError()
}
