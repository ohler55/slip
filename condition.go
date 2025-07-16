// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

// ConditionSymbol is the symbol with a value of "condition".
const ConditionSymbol = Symbol("condition")

// NewCondition returns a condition.
func NewCondition(report Object) Object {
	c := FindClass("condition")
	obj := c.MakeInstance()

	obj.Init(NewScope(), List{
		Symbol(":report"), report,
	}, 0)
	return obj
}
