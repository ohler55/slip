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

// PanicSerious raises a SeriousPanic (serious-condition) describing a serious
// condition.
func PanicSerious() {
	panic(NewSeriousCondition())
}

// var seriousConditionHierarchy = []Symbol{SeriousConditionSymbol, ConditionSymbol, ConditionSymbol, TrueSymbol}

// func init() {
// 	RegisterCondition("serious-condition", makeSeriousCondition)
// }

// // SeriousCondition is the interface for all serious-conditions including
// // errors and warnings. It has no functions that provide useful information
// // other than to indicate the type is a SeriousCondition which is also an
// // Object.
// type SeriousCondition interface {
// 	Condition

// 	// IsSeriousCondition need not do anything other than exist.
// 	IsSeriousCondition()
// }

// // SeriousConditionObj is used to gather a stack trace when panic occurs.
// type SeriousConditionObj struct {
// 	ConditionObj
// }

// // IsCondition indicates SeriousConditionObj is a Condition.
// func (c *SeriousConditionObj) IsCondition() {
// }

// // IsSeriousCondition indicates SeriousConditionObj is a Condition.
// func (c *SeriousConditionObj) IsSeriousCondition() {
// }

// // Equal returns true if this Object and the other are equal in value.
// func (c *SeriousConditionObj) Equal(other Object) bool {
// 	return c == other
// }

// // Eval the object.
// func (c *SeriousConditionObj) Eval(s *Scope, depth int) Object {
// 	return c
// }

// func makeSeriousCondition(args List) Condition {
// 	return &SeriousConditionObj{ConditionObj: ConditionObj{hierarchy: seriousConditionHierarchy}}
// }
