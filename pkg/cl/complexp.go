// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Complexp{Function: slip.Function{Name: "complexp", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "complexp",
			Args: []*slip.DocArg{
				{
					Name: "object",
					Type: "object",
					Text: "The object to check.",
				},
			},
			Return: "boolean",
			Text:   `__complexp__ returns _true_ if _object_ is a complex.`,
			Examples: []string{
				"(complexp 4) => t",
				"(complexp t) => nil",
			},
		}, &slip.CLPkg)
}

// Complexp represents the complexp function.
type Complexp struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Complexp) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 1)
	if _, ok := args[0].(slip.Complex); ok {
		return slip.True
	}
	return nil
}
