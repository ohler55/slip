// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import "github.com/ohler55/slip"

// SimpleWarningSymbol is the symbol with a value of "simple-error".
const SimpleWarningSymbol = slip.Symbol("simple-warning")

var simpleWarningHierarchy = []slip.Symbol{
	SimpleWarningSymbol,
	SimpleConditionSymbol,
	slip.WarningSymbol,
	slip.ConditionSymbol,
	slip.TrueSymbol,
}

func init() {
	slip.RegisterCondition("simple-warning", makeSimpleWarning)
}

// SimpleWarning is the interface for all simple-errors. It has no functions that provide
// useful information other than to indicate the type is a SimpleWarning which is
// also an Object.
type SimpleWarning interface {
	SimpleCondition
	slip.Warning

	// IsSimpleWarning need not do anything other than exist.
	IsSimpleWarning()
}

// SimpleWarningObj is used to gather a stack trace when panic occurs.
type SimpleWarningObj struct {
	slip.WarningObj
	SimpleFormatterEmbed
}

// IsSimpleCondition indicates SimpleConditionObj is a Condition.
func (sw *SimpleWarningObj) IsSimpleCondition() {
}

// IsSimpleWarning indicates SimpleWarningObj is a Error.
func (sw *SimpleWarningObj) IsSimpleWarning() {
}

// IsWarning indicates WarningObj is a Error.
func (sw *SimpleWarningObj) IsWarning() {
}

// Equal returns true if this Object and the other are equal in value.
func (sw *SimpleWarningObj) Equal(other slip.Object) bool {
	return sw == other
}

// Eval the object.
func (sw *SimpleWarningObj) Eval(s *slip.Scope, depth int) slip.Object {
	return sw
}

// Error returns the error message.
func (sw *SimpleWarningObj) Error() string {
	return sw.Output()
}

// NewSimpleWarning returns a SimpleWarningObj object that can then be used with a call to panic.
func NewSimpleWarning(s *slip.Scope, ctrl string, args ...slip.Object) *SimpleWarningObj {
	var sw SimpleWarningObj
	sw.SetHierarchy(simpleWarningHierarchy)
	sw.Init(s, ctrl, args)
	return &sw
}

// PanicSimpleWarning raises a SimpleWarningObj instance.
func PanicSimpleWarning(s *slip.Scope, ctrl string, args ...slip.Object) {
	panic(NewSimpleWarning(s, ctrl, args...))
}

func makeSimpleWarning(args slip.List) slip.Condition {
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
	return NewSimpleWarning(nil, ctrl, fargs...)
}
