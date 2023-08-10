// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

// CellErrorSymbol is the symbol with a value of "cell-error".
const CellErrorSymbol = Symbol("cell-error")

// CellError is the interface for all cell-errors.
type CellError interface {
	Error

	// IsCellError need not do anything other than exist.
	IsCellError()

	// Name returns the name associated with the error.
	Name() string
}
