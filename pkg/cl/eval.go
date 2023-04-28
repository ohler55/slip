// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Eval{Function: slip.Function{Name: "eval", Args: args, SkipEval: []bool{true}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "eval",
			Args: []*slip.DocArg{
				{
					Name: "form",
					Type: "form",
					Text: "The form to evaluate.",
				},
			},
			Return: "nil",
			Text:   `__eval__ evaluates the _form_.`,
			Examples: []string{
				`(eval (+ 1 2)) => 3"`,
				`(eval '(+ 1 2)) => 3"`,
			},
		}, &slip.CLPkg)
}

// Eval represents the eval function.
type Eval struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Eval) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, 1)
	result = slip.EvalArg(s, args, 0, depth+1)

	return s.Eval(result, depth+1)
}
