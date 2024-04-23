// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Vectorp{Function: slip.Function{Name: "vectorp", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "vectorp",
			Args: []*slip.DocArg{
				{
					Name: "object",
					Type: "object",
					Text: "The object to check.",
				},
			},
			Return: "boolean",
			Text:   `__vectorp__ returns _true_ if _object_ is a vector.`,
			Examples: []string{
				"(vectorp #(1 2 3)) => t",
				`(vectorp "abc") => t`,
				"(vectorp 5) => nil",
			},
		}, &slip.CLPkg)
}

// Vectorp represents the vectorp function.
type Vectorp struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Vectorp) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 1)
	switch args[0].(type) {
	case *slip.Vector, slip.String:
		return slip.True
	}
	return nil
}
