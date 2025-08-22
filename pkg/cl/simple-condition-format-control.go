// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

const formatControlSymbol = slip.Symbol("format-control")

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := SimpleConditionFormatControl{
				Function: slip.Function{Name: "simple-condition-format-control", Args: args},
			}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "simple-condition-format-control",
			Args: []*slip.DocArg{
				{
					Name: "condition",
					Type: "simple-condition",
					Text: "The simple-condition or subclass of simple-condition to get the format-control of.",
				},
			},
			Return: "object",
			Text: `__simple-condition-format-control__ returns the value of the _format-control_ slot in the
_condition_ which must be of simple _simple-condition_ or inherit from _simple-condition_.`,
			Examples: []string{
				`(simple-condition-format-control (make-condition 'simple-condition :format-control "test")) => test)`,
			},
		}, &slip.CLPkg)
}

// SimpleConditionFormatControl represents the simple-condition-format-control function.
type SimpleConditionFormatControl struct {
	slip.Function
}

// Call the function with the control provided.
func (f *SimpleConditionFormatControl) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	if ci, ok := args[0].(slip.Instance); !ok || !ci.IsA("simple-condition") {
		slip.TypePanic(s, depth, "simple-condition", args[0], "simple-condition")
	} else {
		result, _ = ci.SlotValue(formatControlSymbol)
	}
	return
}
