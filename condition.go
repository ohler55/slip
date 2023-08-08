// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

// ConditionSymbol is the symbol with a value of "condition".
const ConditionSymbol = Symbol("condition")

// Condition is the interface for all conditions including errors and
// warnings. It has no functions that provide useful information other than to
// indicate the type is a Condition which is also an Object.
type Condition interface {
	Object
	// IsCondition need not do anything other than exist.
	IsCondition()
}
