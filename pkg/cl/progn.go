// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Progn{Function: slip.Function{Name: "progn", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "progn",
			Args: []*slip.DocArg{
				{Name: "&rest"},
				{
					Name: "forms",
					Type: "object",
					Text: "The forms to evaluate.",
				},
			},
			Return: "object",
			Text:   `__progn__ evaluates all _forms_ and returns the value of the last form.`,
			Examples: []string{
				"(progn 1 (+ 1 1) 3) => 3",
			},
		}, &slip.CLPkg)
}

// Progn represents the progn function.
type Progn struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Progn) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	if 0 < len(args) {
		result = args[len(args)-1]
	}
	return
}
