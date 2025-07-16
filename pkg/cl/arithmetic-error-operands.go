// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

const operandsSymbol = slip.Symbol("operands")

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := ArithmeticErrorOperands{Function: slip.Function{Name: "arithmetic-error-operands", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "arithmetic-error-operands",
			Args: []*slip.DocArg{
				{
					Name: "condition",
					Type: "arithmetic-error",
					Text: "The arithmetic-error or subclass of arithmetic-error to get the operands of.",
				},
			},
			Return: "object",
			Text: `__arithmetic-error-operands__ returns the value of the _operands_ slot in the _condition_
which must be of arithmetic _arithmetic-error_ or inherit from _arithmetic-error_.`,
			Examples: []string{
				`(arithmetic-error-operands (make-condition 'arithmetic-error :operands 'test)) => test)`,
			},
		}, &slip.CLPkg)
}

// ArithmeticErrorOperands represents the arithmetic-error-operands function.
type ArithmeticErrorOperands struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *ArithmeticErrorOperands) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, 1)
	if ci, ok := args[0].(slip.Instance); !ok || !ci.IsA("arithmetic-error") {
		slip.PanicType("arithmetic-error", args[0], "arithmetic-error")
	} else {
		var has bool
		if result, has = ci.SlotValue(operandsSymbol); !has {
			slip.PanicUnboundSlot(ci, operandsSymbol, "")
		}
	}
	return
}
