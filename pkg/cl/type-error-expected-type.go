// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

const expectedTypeSymbol = slip.Symbol("expected-type")

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := TypeErrorExpectedType{Function: slip.Function{Name: "type-error-expected-type", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "type-error-expected-type",
			Args: []*slip.DocArg{
				{
					Name: "condition",
					Type: "type-error",
					Text: "The type-error or subclass of type-error to get the expected-type of.",
				},
			},
			Return: "object",
			Text: `__type-error-expected-type__ returns the value of the _expected-type_ slot in the _condition_
which must be of type _type-error_ or inherit from _type-error_. If there are multiple _expected-type_ values
a values type is returned.`,
			Examples: []string{
				`(type-error-expected-type (make-condition 'type-error :expected-type 'test)) => test)`,
			},
		}, &slip.CLPkg)
}

// TypeErrorExpectedType represents the type-error-expected-type function.
type TypeErrorExpectedType struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *TypeErrorExpectedType) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, 1)
	switch cond := args[0].(type) {
	case slip.TypeError:
		ta := cond.ExpectedTypes()
		if len(ta) == 1 {
			return ta[0]
		}
		result = slip.Values(ta)
	case slip.Instance:
		var has bool
		if result, has = cond.SlotValue(expectedTypeSymbol); !has {
			slip.PanicUnboundSlot(args[0], expectedTypeSymbol, "")
		}
	default:
		slip.PanicUnboundSlot(args[0], expectedTypeSymbol, "")
	}
	return
}
