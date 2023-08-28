// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import "github.com/ohler55/slip"

// SimpleTypeErrorSymbol is the symbol with a value of "simple-error".
const SimpleTypeErrorSymbol = slip.Symbol("simple-type-error")

var simpleTypeErrorHierarchy = []slip.Symbol{
	SimpleTypeErrorSymbol,
	SimpleConditionSymbol,
	slip.TypeErrorSymbol,
	slip.ErrorSymbol,
	slip.SeriousConditionSymbol,
	slip.ConditionSymbol,
	slip.TrueSymbol,
}

func init() {
	slip.RegisterCondition("simple-type-error", makeSimpleTypeError)
}

// SimpleTypeError is the interface for all simple-errors. It has no functions that provide
// useful information other than to indicate the type is a SimpleTypeError which is
// also an Object.
type SimpleTypeError interface {
	SimpleCondition
	slip.TypeError

	// IsSimpleTypeError need not do anything other than exist.
	IsSimpleTypeError()
}

// SimpleTypeErrorObj is used to gather a stack trace when panic occurs.
type SimpleTypeErrorObj struct {
	slip.Panic
	SimpleFormatterEmbed
	datum         slip.Object
	context       slip.Object
	expectedTypes slip.List
}

// IsSimpleCondition indicates SimpleConditionObj is a Condition.
func (ste *SimpleTypeErrorObj) IsSimpleCondition() {
}

// IsSimpleTypeError indicates SimpleTypeErrorObj is a Error.
func (ste *SimpleTypeErrorObj) IsSimpleTypeError() {
}

// IsTypeError need not do anything other than exist.
func (ste *SimpleTypeErrorObj) IsTypeError() {
}

// Datum is the object provided.
func (ste *SimpleTypeErrorObj) Datum() slip.Object {
	return ste.datum
}

// Context is the context provided.
func (ste *SimpleTypeErrorObj) Context() slip.Object {
	return ste.context
}

// ExpectedTypes are the expected types. This deviates from common LSIP
// where this is only one expected-type.
func (ste *SimpleTypeErrorObj) ExpectedTypes() slip.List {
	return ste.expectedTypes
}

// Equal returns true if this Object and the other are equal in value.
func (ste *SimpleTypeErrorObj) Equal(other slip.Object) bool {
	return ste == other
}

// Eval the object.
func (ste *SimpleTypeErrorObj) Eval(s *slip.Scope, depth int) slip.Object {
	return ste
}

// Error returns the error message.
func (ste *SimpleTypeErrorObj) Error() string {
	return ste.output
}

// NewSimpleTypeError returns a SimpleTypeErrorObj object that can then be used with a call to panic.
func NewSimpleTypeError(s *slip.Scope, ctrl string, args ...slip.Object) *SimpleTypeErrorObj {
	var ste SimpleTypeErrorObj
	ste.SetHierarchy(simpleTypeErrorHierarchy)
	ste.Init(s, ctrl, args)
	return &ste
}

// PanicSimpleTypeError raises a SimpleTypeErrorObj instance.
func PanicSimpleTypeError(s *slip.Scope, ctrl string, args ...slip.Object) {
	panic(NewSimpleTypeError(s, ctrl, args...))
}

func makeSimpleTypeError(args slip.List) slip.Condition {
	var (
		ctrl    string
		fargs   slip.List
		datum   slip.Object
		wants   slip.List
		context slip.Object
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
		case ":datum":
			datum = v
		case ":expected-type":
			switch tv := v.(type) {
			case slip.List:
				for _, e := range tv {
					switch te := e.(type) {
					case slip.String:
						wants = append(wants, slip.Symbol(te))
					case slip.Symbol:
						wants = append(wants, te)
					}

				}
			case slip.String:
				wants = slip.List{slip.Symbol(tv)}
			case slip.Symbol:
				wants = slip.List{tv}
			}
		case ":context":
			context = v

		}
	}
	cond := NewSimpleTypeError(nil, ctrl, fargs...)
	cond.datum = datum
	cond.expectedTypes = wants
	cond.context = context
	return cond
}
