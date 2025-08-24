// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

const operationSymbol = slip.Symbol("operation")

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := ArithmeticErrorOperation{Function: slip.Function{Name: "arithmetic-error-operation", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "arithmetic-error-operation",
			Args: []*slip.DocArg{
				{
					Name: "condition",
					Type: "arithmetic-error",
					Text: "The arithmetic-error or subclass of arithmetic-error to get the operation of.",
				},
			},
			Return: "object",
			Text: `__arithmetic-error-operation__ returns the value of the _operation_ slot in the _condition_
which must be of arithmetic _arithmetic-error_ or inherit from _arithmetic-error_.`,
			Examples: []string{
				`(arithmetic-error-operation (make-condition 'arithmetic-error :operation 'test)) => test)`,
			},
		}, &slip.CLPkg)
}

// ArithmeticErrorOperation represents the arithmetic-error-operation function.
type ArithmeticErrorOperation struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *ArithmeticErrorOperation) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	if ci, ok := args[0].(slip.Instance); !ok || !ci.IsA("arithmetic-error") {
		slip.TypePanic(s, depth, "arithmetic-error", args[0], "arithmetic-error")
	} else {
		result, _ = ci.SlotValue(operationSymbol)
	}
	return
}
