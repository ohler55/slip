// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

// SeriousConditionSymbol is the symbol with a value of "serious-condition".
const SeriousConditionSymbol = Symbol("serious-condition")

// SeriousCondition is the interface for all serious-conditions including
// errors and warnings. It has no functions that provide useful information
// other than to indicate the type is a SeriousCondition which is also an
// Object.
type SeriousCondition interface {
	Condition

	// IsSeriousCondition need not do anything other than exist.
	IsSeriousCondition()
}
