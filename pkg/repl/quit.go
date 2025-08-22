// Copyright (c) 2023, Peter Ohler, All rights reserved.

package repl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Quit{Function: slip.Function{Name: "quit", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "quit",
			Args: []*slip.DocArg{},
			Text: `__quit__ exits the REPL or the application.`,
		}, &Pkg)
}

// Quit represents the quit function.
type Quit struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *Quit) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	cond := slip.ErrorNew(s, depth, "").(slip.Instance)
	p := slip.WrapError(s, cond, "quit", nil)
	p.Fatal = true
	panic(p)
}
