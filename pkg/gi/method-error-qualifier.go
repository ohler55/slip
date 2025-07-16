// Copyright (c) 2023, Peter Ohler, All rights reserved.

package gi

import (
	"github.com/ohler55/slip"
)

const qualifierSymbol = slip.Symbol("qualifier")

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := MethodErrorQualifier{Function: slip.Function{Name: "method-error-qualifier", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "method-error-qualifier",
			Args: []*slip.DocArg{
				{
					Name: "condition",
					Type: "method-error",
					Text: "The method-error or subqualifier of method-error to get the qualifier of.",
				},
			},
			Return: "object",
			Text: `__method-error-qualifier__ returns the value of the _qualifier_ slot in the _condition_
which must be of method _method-error_ or inherit from _method-error_.`,
			Examples: []string{
				`(method-error-qualifier (make-condition 'method-error :qualifier 'test)) => test)`,
			},
		}, &Pkg)
}

// MethodErrorQualifier represents the method-error-qualifier function.
type MethodErrorQualifier struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *MethodErrorQualifier) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, 1)
	if ci, ok := args[0].(slip.Instance); !ok || !ci.IsA("method-error") {
		slip.PanicType("method-error", args[0], "method-error")
	} else {
		var has bool
		if result, has = ci.SlotValue(qualifierSymbol); !has {
			slip.PanicUnboundSlot(ci, qualifierSymbol, "")
		}
	}
	return
}
