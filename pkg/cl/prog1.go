// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Prog1{Function: slip.Function{Name: "prog1", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "prog1",
			Args: []*slip.DocArg{
				{Name: "&rest"},
				{
					Name: "forms",
					Type: "object",
					Text: "The forms to evaluate.",
				},
			},
			Return: "object",
			Text:   `__prog1__ evaluates all _forms_ and returns the value of the first form.`,
			Examples: []string{
				"(prog1 1 (+ 1 1) 3) => 1",
			},
		}, &slip.CLPkg)
}

// Prog1 represents the prog1 function.
type Prog1 struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Prog1) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, -1)

	return args[0]
}
