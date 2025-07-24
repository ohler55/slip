// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

const datumSymbol = slip.Symbol("datum")

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := TypeErrorDatum{Function: slip.Function{Name: "type-error-datum", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "type-error-datum",
			Args: []*slip.DocArg{
				{
					Name: "condition",
					Type: "type-error",
					Text: "The type-error or subclass of type-error to get the datum of.",
				},
			},
			Return: "object",
			Text: `__type-error-datum__ returns the value of the _datum_ slot in the _condition_
which must be of type _type-error_ or inherit from _type-error_.`,
			Examples: []string{
				`(type-error-datum (make-condition 'type-error :datum 'test)) => test)`,
			},
		}, &slip.CLPkg)
}

// TypeErrorDatum represents the type-error-datum function.
type TypeErrorDatum struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *TypeErrorDatum) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, 1)
	if ci, ok := args[0].(slip.Instance); !ok || !ci.IsA("type-error") {
		slip.PanicType("type-error", args[0], "type-error")
	} else {
		result, _ = ci.SlotValue(datumSymbol)
	}
	return
}
