// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Atom{Function: slip.Function{Name: "atom", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "atom",
			Args: []*slip.DocArg{
				{
					Name: "object",
					Type: "object",
					Text: "The object to check.",
				},
			},
			Return: "nil",
			Text:   `__atom__ returns _true_ if _object_ is an atom otherwise nil is returned.`,
			Examples: []string{
				"(atom 1.2) => t",
				"(atom '(1 2)) => nil",
			},
		}, &slip.CLPkg)
}

// Atom represents the atom function.
type Atom struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Atom) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	if len(args) != 1 {
		slip.PanicArgCount(f, 1, 1)
	}
	if list, ok := args[0].(slip.List); ok && 0 < len(list) {
		return nil
	}
	return slip.True
}
