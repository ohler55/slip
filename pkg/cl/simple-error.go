// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import "github.com/ohler55/slip"

// SimpleErrorSymbol is the symbol with a value of "simple-error".
const SimpleErrorSymbol = slip.Symbol("simple-error")

var simpleErrorHierarchy = []slip.Symbol{
	SimpleErrorSymbol,
	SimpleConditionSymbol,
	slip.ErrorSymbol,
	slip.ConditionSymbol,
	slip.TrueSymbol,
}

func init() {
	slip.RegisterCondition("simple-error", makeSimpleError)
}

// SimpleError is the interface for all simple-errors. It has no functions that provide
// useful information other than to indicate the type is a SimpleError which is
// also an Object.
type SimpleError interface {
	SimpleCondition
	slip.Error

	// IsSimpleError need not do anything other than exist.
	IsSimpleError()
}

// SimpleErrorObj is used to gather a stack trace when panic occurs.
type SimpleErrorObj struct {
	slip.Panic
	SimpleFormatterEmbed
}

// IsSimpleCondition indicates SimpleConditionObj is a Condition.
func (se *SimpleErrorObj) IsSimpleCondition() {
}

// IsSimpleError indicates SimpleErrorObj is a Error.
func (se *SimpleErrorObj) IsSimpleError() {
}

// Equal returns true if this Object and the other are equal in value.
func (se *SimpleErrorObj) Equal(other slip.Object) bool {
	return se == other
}

// Eval the object.
func (se *SimpleErrorObj) Eval(s *slip.Scope, depth int) slip.Object {
	return se
}

// Error returns the error message.
func (se *SimpleErrorObj) Error() string {
	return se.output
}

// NewSimpleError returns a SimpleErrorObj object that can then be used with a call to panic.
func NewSimpleError(s *slip.Scope, ctrl string, args ...slip.Object) *SimpleErrorObj {
	var se SimpleErrorObj
	se.SetHierarchy(simpleErrorHierarchy)
	se.Init(s, ctrl, args)
	return &se
}

// PanicSimpleError raises a SimpleErrorObj instance.
func PanicSimpleError(s *slip.Scope, ctrl string, args ...slip.Object) {
	panic(NewSimpleError(s, ctrl, args...))
}

func makeSimpleError(args slip.List) slip.Condition {
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
	return NewSimpleError(nil, ctrl, fargs...)
}
