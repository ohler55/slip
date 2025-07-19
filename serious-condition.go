// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

// SeriousConditionSymbol is the symbol with a value of "serious-condition".
const SeriousConditionSymbol = Symbol("serious-condition")

// NewSeriousCondition creates a serious-condition.
func NewSeriousCondition() Object {
	c := FindClass("serious-condition")
	obj := c.MakeInstance()
	obj.Init(NewScope(), List{}, 0)
	return obj
}

// PanicSeriousCondition raises a serious-condition describing a serious
// condition.
func PanicSeriousCondition() {
	panic(NewSeriousCondition())
}
