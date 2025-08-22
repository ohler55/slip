// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Ninth{Function: slip.Function{Name: "ninth", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "ninth",
			Args: []*slip.DocArg{
				{
					Name: "list",
					Type: "list|cons",
					Text: "The value to take the ninth element of.",
				},
			},
			Return: "object",
			Text:   `__ninth__ returns the ninth element in a _list_ or _nil_ if there is no ninth element.`,
			Examples: []string{
				"(ninth nil) => nil",
				"(ninth '(a b c d e f g h . i) => i",
				"(ninth '(a b c d e f g h i)) => i",
			},
		}, &slip.CLPkg)
}

// Ninth represents the ninth function.
type Ninth struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Ninth) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	return callN(s, f, args, 8, depth)
}

// Place a value in the ninth position of a list or cons.
func (f *Ninth) Place(s *slip.Scope, args slip.List, value slip.Object) {
	placeN(s, f, args, 8, value)
}
