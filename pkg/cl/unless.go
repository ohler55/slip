// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Unless{Function: slip.Function{Name: "unless", Args: args, SkipEval: []bool{true}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "unless",
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
			Text: `__unless__ evaluates the _forms_ if _test-form_ evaluates to _nil_ and returns
the result of the last form evaluated.`,
			Examples: []string{
				"(unless t (+ 1 2)) => 3",
				"(unless nil 3) => nil",
			},
		}, &slip.CLPkg)
}

// Unless represents the unless function.
type Unless struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Unless) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	if len(args) < 1 {
		slip.PanicArgCount(f, 1, -1)
	}
	result = nil
	d2 := depth + 1
	pos := 0
	if slip.EvalArg(s, args, pos, d2) == nil {
		for pos++; pos < len(args); pos++ {
			result = slip.EvalArg(s, args, pos, d2)
		}
	}
	return
}
