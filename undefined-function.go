// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

// UndefinedFunctionSymbol is the symbol with a value of "undefined-function".
const UndefinedFunctionSymbol = Symbol("undefined-function")

// UndefinedFunction is the interface for all undefined-functions.
type UndefinedFunction interface {
	CellError

	// IsUndefinedFunction need not do anything other than exist.
	IsUndefinedFunction()
}
