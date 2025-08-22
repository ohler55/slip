// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := MultipleValueProg1{
				Function: slip.Function{Name: "multiple-value-prog1", Args: args, SkipEval: []bool{true}},
			}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "multiple-value-prog1",
			Args: []*slip.DocArg{
				{
					Name: "first-form",
					Type: "form",
					Text: "A form to evaluate for the return values.",
				},
				{Name: "&rest"},
				{
					Name: "forms",
					Type: "form",
					Text: "The forms to evaluate after the first form.",
				},
			},
			Return: "object",
			Text: `__multiple-value-prog1__ evaluates the _first-form_ and saves the results
to be used as the return value or values. The rest of the _forms_ are then evaluated and the
return values of those evaluations are ignore.`,
			Examples: []string{
				"(multiple-value-prog1 (floor 5 3) (+ 2 3)) => (1 2)",
			},
		}, &slip.CLPkg)
}

// MultipleValueProg1 represents the multiple-value-prog1 function.
type MultipleValueProg1 struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *MultipleValueProg1) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 1, -1)
	d2 := depth + 1
	ff := args[0]
	if list, ok := ff.(slip.List); ok {
		ff = slip.ListToFunc(s, list, d2)
	}
	result = s.Eval(ff, d2)
	for _, arg := range args[1:] {
		if list, ok := arg.(slip.List); ok {
			arg = slip.ListToFunc(s, list, d2)
		}
		_ = s.Eval(arg, d2)
	}
	return
}
