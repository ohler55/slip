// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := IgnoreErrors{Function: slip.Function{Name: "ignore-errors", Args: args, SkipEval: []bool{true}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "ignore-errors",
			Args: []*slip.DocArg{
				{Name: "&rest"},
				{
					Name: "forms",
					Type: "object",
					Text: "The forms to evaluate.",
				},
			},
			Return: "object|values",
			Text: `__ignore-errors__ catches any raised object from a call to the __panic__ function. If a
panic is recovered from then the return is two values, _nil_ and the condition that was recovered. If no
condition is raised then the result of the forms is returned.`,
			Examples: []string{
				`(ignore-errors (+ 1 2)) => 3`,
				`(ignore-errors (/ 1 0)) => nil, #<ARITHMETIC-ERROR 12345>`,
			},
		}, &slip.CLPkg)
}

// IgnoreErrors represents the ignore-errors function.
type IgnoreErrors struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *IgnoreErrors) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	defer func() {
		if rec := recover(); rec != nil {
			ro, _ := rec.(slip.Condition)
			result = slip.Values{nil, ro}
		}
	}()
	d2 := depth + 1
	for i := range args {
		result = slip.EvalArg(s, args, i, d2)
	}
	return
}
