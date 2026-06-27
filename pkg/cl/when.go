// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := When{
				Function: slip.Function{Name: "when", Args: args, SkipEval: []bool{true}},
				preProv:  slip.Provenance,
			}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "when",
			Args: []*slip.DocArg{
				{
					Name: "test-form",
					Type: "object",
					Text: "The form to evaluate as the test.",
				},
				{Name: "&rest"},
				{
					Name: "forms",
					Type: "object",
					Text: "The forms to evaluate.",
				},
			},
			Return: "object",
			Text: `__when__ evaluates the _forms_ if _test-form_ evaluates to non-_nil_ and returns
the result of the last form evaluated.`,
			Examples: []string{
				"(when t (+ 1 2)) => 3",
				"(when nil 3) => nil",
			},
		}, &slip.CLPkg)
}

// When represents the when function.
type When struct {
	slip.Function
	preProv bool
}

// Call the function with the arguments provided.
func (f *When) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 1, -1)
	result = nil
	d2 := depth + 1
	if f.preProv {
		for i, a := range args {
			if list, ok := a.(slip.List); ok {
				args[i] = slip.ListToFunc(s, list, d2)
			}
		}
		f.preProv = false
	}
	pos := 0
	if slip.EvalArg(s, args, pos, d2) != nil {
		for pos++; pos < len(args); pos++ {
			result = slip.EvalArg(s, args, pos, d2)
		}
	}
	return
}
