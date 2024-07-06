// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Rationalp{Function: slip.Function{Name: "rationalp", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "rationalp",
			Args: []*slip.DocArg{
				{
					Name: "object",
					Type: "object",
					Text: "The object to check.",
				},
			},
			Return: "boolean",
			Text:   `__rationalp__ returns _true_ if _object_ is a rational.`,
			Examples: []string{
				"(rationalp 4) => t",
				"(rationalp 4/3) => t",
				"(rationalp 5.1) => nil",
			},
		}, &slip.CLPkg)
}

// Rationalp represents the rationalp function.
type Rationalp struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Rationalp) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 1)
	if _, ok := args[0].(slip.Rational); ok {
		return slip.True
	}
	return nil
}
