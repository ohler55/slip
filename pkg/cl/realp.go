// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Realp{Function: slip.Function{Name: "realp", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "realp",
			Args: []*slip.DocArg{
				{
					Name: "object",
					Type: "object",
					Text: "The object to check.",
				},
			},
			Return: "boolean",
			Text:   `__realp__ returns _true_ if _object_ is a real.`,
			Examples: []string{
				"(realp 4) => t",
				"(realp 5.1) => nil",
			},
		}, &slip.CLPkg)
}

// Realp represents the realp function.
type Realp struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Realp) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 1)
	if _, ok := args[0].(slip.Real); ok {
		return slip.True
	}
	return nil
}
