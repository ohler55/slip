// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Or{Function: slip.Function{Name: "or", Args: args, SkipEval: []bool{true}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "or",
			Args: []*slip.DocArg{
				{Name: "&rest"},
				{
					Name: "forms",
					Type: "object",
					Text: "The forms to evaluate.",
				},
			},
			Return: "nil",
			Text: `__or__ returns the result of the first form that evaluates to non-_nil_
otherwise _nil_ is returned.`,
			Examples: []string{
				"(or t nil) => t",
				"(or nil) => nil",
			},
		}, &slip.CLPkg)
}

// Or represents the or function.
type Or struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *Or) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	result = nil
	d2 := depth + 1
	for i := range args {
		if result = f.EvalArg(s, args, i, d2); result != nil {
			break
		}
	}
	return
}
