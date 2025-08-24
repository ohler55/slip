// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Integerp{Function: slip.Function{Name: "integerp", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "integerp",
			Args: []*slip.DocArg{
				{
					Name: "object",
					Type: "object",
					Text: "The object to check.",
				},
			},
			Return: "nil",
			Text:   `__integerp__ returns _true_ if _object_ is an integer.`,
			Examples: []string{
				"(integerp 4) => t",
				"(integerp 5.1) => nil",
			},
		}, &slip.CLPkg)
}

// Integerp represents the integerp function.
type Integerp struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Integerp) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	if _, ok := args[0].(slip.Integer); ok {
		return slip.True
	}
	return nil
}
