// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := If{
				Function: slip.Function{Name: "if", Args: args, SkipEval: []bool{true}},
				preProv:  slip.Provenance,
			}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "if",
			Args: []*slip.DocArg{
				{
					Name: "test-form",
					Type: "object",
					Text: "The form to evaluate as the test.",
				},
				{
					Name: "then-form",
					Type: "object",
					Text: "The form to evaluate if _test-form_ evaluates to non-_nil_.",
				},
				{Name: "&optional"},
				{
					Name: "else-form",
					Type: "object",
					Text: "The form to evaluate if _test-form_ evaluates to _nil_.",
				},
			},
			Return: "object",
			Text: `__if__ evaluates the _then-forms_ if _test-form_ evaluates to non-_nil_ and returns
the result of the last form evaluated. If _else-form_ is present and _test-form_ evaluates to _nil_
then _else-form_ is evaluated and the result returned.`,
			Examples: []string{
				"(if t (+ 1 2) (+ 2 3)) => 3",
				"(if nil (+ 1 2) (+ 2 3)) => 5",
			},
		}, &slip.CLPkg)
}

// If represents the if function.
type If struct {
	slip.Function
	preProv bool
}

// Call the function with the arguments provided.
func (f *If) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 2, 3)
	d2 := depth + 1
	if f.preProv {
		for i := 1; i < len(args); i++ {
			if list, ok := args[i].(slip.List); ok {
				args[i] = slip.ListToFunc(s, list, d2)
			}
		}
		f.preProv = false
	}
	if slip.EvalArg(s, args, 0, d2) != nil {
		result = slip.EvalArg(s, args, 1, d2)
	} else if 2 < len(args) {
		result = slip.EvalArg(s, args, 2, d2)
	}
	return
}
