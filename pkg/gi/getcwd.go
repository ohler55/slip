// Copyright (c) 2026, Peter Ohler, All rights reserved.

package gi

import (
	"os"

	"github.com/ohler55/slip"
)

func defGetcwd() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Getcwd{Function: slip.Function{Name: "getcwd", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name:   "getcwd",
			Args:   []*slip.DocArg{},
			Return: "string",
			Text:   `__getcwd__ return the current directory.`,
		}, &Pkg)
}

// Getcwd represents the getcwd function.
type Getcwd struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Getcwd) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 0, 0)
	dir, _ := os.Getwd()

	return slip.String(dir)
}
