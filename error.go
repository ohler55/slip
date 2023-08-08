// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

// ErrorSymbol is the symbol with a value of "error".
const ErrorSymbol = Symbol("error")

// Error is the interface for all errors. It has no functions that provide
// useful information other than to indicate the type is an Error which is
// also an Object.
type Error interface {
	SeriousCondition

	// IsError need not do anything other than exist.
	IsError()

	// AppendToStack appends a function name and argument to the stack.
	AppendToStack(name string, args List)
}
