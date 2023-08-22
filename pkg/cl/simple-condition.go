// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import "github.com/ohler55/slip"

// SimpleConditionSymbol is the symbol with a value of "simple-condition".
const SimpleConditionSymbol = slip.Symbol("simple-condition")

var simpleConditionHierarchy = []slip.Symbol{SimpleConditionSymbol, slip.ConditionSymbol, slip.TrueSymbol}

func init() {
	slip.RegisterCondition("simple-condition", makeSimpleCondition)
}

// SimpleCondition is the interface for all simple-conditions. It has no functions that provide
// useful information other than to indicate the type is a SimpleCondition which is
// also an Object.
type SimpleCondition interface {
	slip.Condition
	SimpleFormatter

	// IsSimpleCondition need not do anything other than exist.
	IsSimpleCondition()
}

// SimpleConditionObj is used to gather a stack trace when panic occurs.
type SimpleConditionObj struct {
	slip.ConditionObj
	SimpleFormatterEmbed
}

// IsSimpleCondition indicates SimpleConditionObj is a Condition.
func (sc *SimpleConditionObj) IsSimpleCondition() {
}

// Equal returns true if this Object and the other are equal in value.
func (sc *SimpleConditionObj) Equal(other slip.Object) bool {
	return sc == other
}

// Eval the object.
func (sc *SimpleConditionObj) Eval(s *slip.Scope, depth int) slip.Object {
	return sc
}

// Error returns the condition message.
func (sc *SimpleConditionObj) Error() string {
	return sc.output
}

// NewSimpleCondition returns a SimpleConditionObj object that can then be used with a call to panic.
func NewSimpleCondition(s *slip.Scope, ctrl string, args ...slip.Object) *SimpleConditionObj {
	var sc SimpleConditionObj
	sc.SetHierarchy(simpleConditionHierarchy)
	sc.ctrl = ctrl
	sc.args = args
	sc.formOutput(s)
	return &sc
}

// NewSimpleConditionObj returns a SimpleConditionObj object that can then be used with a call to panic.
func NewSimpleConditionObj(s *slip.Scope, ctrl string, args ...slip.Object) {
	panic(NewSimpleCondition(s, ctrl, args...))
}

func makeSimpleCondition(args slip.List) slip.Condition {
	var (
		ctrl  string
		fargs slip.List
	)
	for k, v := range slip.ParseInitList(args) {
		switch k {
		case ":format-control":
			if ss, ok := v.(slip.String); ok {
				ctrl = string(ss)
			} else {
				slip.PanicType(k, v, "string")
			}
		case ":format-arguments":
			var ok bool
			if fargs, ok = v.(slip.List); !ok {
				slip.PanicType(k, v, "list")
			}
		}
	}
	return NewSimpleCondition(nil, ctrl, fargs)
}
