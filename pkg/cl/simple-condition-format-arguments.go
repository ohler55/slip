// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

const formatArgumentsSymbol = slip.Symbol("format-arguments")

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := SimpleConditionFormatArguments{
				Function: slip.Function{Name: "simple-condition-format-arguments", Args: args},
			}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "simple-condition-format-arguments",
			Args: []*slip.DocArg{
				{
					Name: "condition",
					Type: "simple-condition",
					Text: "The simple-condition or subclass of simple-condition to get the format-arguments of.",
				},
			},
			Return: "object",
			Text: `__simple-condition-format-arguments__ returns the value of the _format-arguments_ slot in the
_condition_ which must be of simple _simple-condition_ or inherit from _simple-condition_.`,
			Examples: []string{
				`(simple-condition-format-arguments (make-condition 'simple-condition :format-arguments "test")) => test)`,
			},
		}, &slip.CLPkg)
}

// SimpleConditionFormatArguments represents the simple-condition-format-arguments function.
type SimpleConditionFormatArguments struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *SimpleConditionFormatArguments) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, 1)
	switch cond := args[0].(type) {
	case slip.Instance:
		var has bool
		if result, has = cond.SlotValue(formatArgumentsSymbol); !has {
			slip.PanicUnboundSlot(args[0], formatArgumentsSymbol, "")
		}
	default:
		slip.PanicUnboundSlot(args[0], formatArgumentsSymbol, "")
	}
	return
}
