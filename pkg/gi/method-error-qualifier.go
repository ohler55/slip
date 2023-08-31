// Copyright (c) 2023, Peter Ohler, All rights reserved.

package gi

import (
	"github.com/ohler55/slip"
)

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
		}, &slip.CLPkg)
}

// MethodErrorQualifier represents the method-error-qualifier function.
type MethodErrorQualifier struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *MethodErrorQualifier) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 1)
	cond, ok := args[0].(slip.MethodError)
	if !ok {
		slip.PanicUnboundSlot(args[0], slip.Symbol("qualifier"), "")
	}
	return cond.Qualifier()
}
