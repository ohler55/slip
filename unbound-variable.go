// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

// UnboundVariableSymbol is the symbol with a value of "unbound-variable".
const UnboundVariableSymbol = Symbol("unbound-variable")

// UnboundVariable is the interface for all unbound-variables.
type UnboundVariable interface {
	CellError

	// IsUnboundVariable need not do anything other than exist.
	IsUnboundVariable()
}
