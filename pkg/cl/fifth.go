// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Fifth{Function: slip.Function{Name: "fifth", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "fifth",
			Args: []*slip.DocArg{
				{
					Name: "list",
					Type: "list|cons",
					Text: "The value to take the fifth element of.",
				},
			},
			Return: "object",
			Text:   `__fifth__ returns the fifth element in a _list_ or _nil_ if there is no fifth element.`,
			Examples: []string{
				"(fifth nil) => nil",
				"(fifth '(a b c d . e) => e",
				"(fifth '(a b c d e)) => e",
			},
		}, &slip.CLPkg)
}

// Fifth represents the fifth function.
type Fifth struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Fifth) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	return callN(f, args, 4)
}

// Place a value in the fifth position of a list or cons.
func (f *Fifth) Place(s *slip.Scope, args slip.List, value slip.Object) {
	placeN(f, args, 4, value)
}
