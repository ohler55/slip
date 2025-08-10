// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Fdefinition{Function: slip.Function{Name: "fdefinition", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "fdefinition",
			Args: []*slip.DocArg{
				{
					Name: "function-name",
					Type: "function-name",
					Text: "The name of a function.",
				},
			},
			Return: "function-definition",
			Text: `__fdefinition__ returns the definition of then function bount to _function-name_.
Use of this function with __setf__ is not supported.`,
			Examples: []string{
				"(fdefinition 'car) => #<built-in car>",
			},
		}, &slip.CLPkg)
}

// Fdefinition represents the fdefinition function.
type Fdefinition struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Fdefinition) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 1)
	sym, _ := args[0].(slip.Symbol)
	fi := slip.FindFunc(string(sym))
	if fi == nil {
		slip.PanicType("function-name", args[0], "function-name")
	}
	return fi
}
