// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Prog2{Function: slip.Function{Name: "prog2", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "prog2",
			Args: []*slip.DocArg{
				{Name: "&rest"},
				{
					Name: "forms",
					Type: "object",
					Text: "The forms to evaluate.",
				},
			},
			Return: "object",
			Text:   `__prog2__ evaluates all _forms_ and returns the value of the first form.`,
			Examples: []string{
				"(prog2 1 (+ 1 1) 3) => 1",
			},
		}, &slip.CLPkg)
}

// Prog2 represents the prog2 function.
type Prog2 struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Prog2) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 2, -1)

	return args[1]
}
