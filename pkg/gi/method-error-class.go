// Copyright (c) 2023, Peter Ohler, All rights reserved.

package gi

import (
	"github.com/ohler55/slip"
)

const classSymbol = slip.Symbol("class")

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := MethodErrorClass{Function: slip.Function{Name: "method-error-class", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "method-error-class",
			Args: []*slip.DocArg{
				{
					Name: "condition",
					Type: "invalid-method-error",
					Text: "The method-error or subclass of method-error to get the class of.",
				},
			},
			Return: "object",
			Text: `__method-error-class__ returns the value of the _class_ slot in the _condition_
which must be of method _method-error_ or inherit from _method-error_.`,
			Examples: []string{
				`(method-error-class (make-condition 'method-error :class 'test)) => test)`,
			},
		}, &Pkg)
}

// MethodErrorClass represents the method-error-class function.
type MethodErrorClass struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *MethodErrorClass) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	if ci, ok := args[0].(slip.Instance); !ok || !ci.IsA("invalid-method-error") {
		slip.TypePanic(s, depth, "invalid-method-error", args[0], "invalid-method-error")
	} else {
		result, _ = ci.SlotValue(classSymbol)
	}
	return
}
