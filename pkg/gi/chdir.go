// Copyright (c) 2026, Peter Ohler, All rights reserved.

package gi

import (
	"os"

	"github.com/ohler55/slip"
)

func defChdir() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Chdir{Function: slip.Function{Name: "chdir", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "chdir",
			Args: []*slip.DocArg{
				{
					Name: "path",
					Type: "string",
					Text: "The directory to change to.",
				},
			},
			Return: "nil",
			Text:   `__chdir__ changes the current directory to _path_.`,
		}, &Pkg)
}

// Chdir represents the chdir function.
type Chdir struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Chdir) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	vs, ok := args[0].(slip.String)
	if !ok {
		slip.TypePanic(s, depth, "path", args[0], "string")
	}
	if err := os.Chdir(string(vs)); err != nil {
		panic(err)
	}
	return nil
}
