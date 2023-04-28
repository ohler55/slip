// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := And{Function: slip.Function{Name: "and", Args: args, SkipEval: []bool{true}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "and",
			Args: []*slip.DocArg{
				{Name: "&rest"},
				{
					Name: "forms",
					Type: "object",
					Text: "The forms to evaluate.",
				},
			},
			Return: "nil",
			Text: `__and__ returns the last evaluated form if all _forms_ evaluate to non-_nil_
otherwise _nil_ is returned.`,
			Examples: []string{
				"(and t nil) => nil",
				"(and t) => t",
			},
		}, &slip.CLPkg)
}

// And represents the and function.
type And struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *And) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	result = slip.True
	d2 := depth + 1
	for i := range args {
		if result = slip.EvalArg(s, args, i, d2); result == nil {
			break
		}
	}
	return
}
