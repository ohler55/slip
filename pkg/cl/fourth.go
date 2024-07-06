// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Fourth{Function: slip.Function{Name: "fourth", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "fourth",
			Args: []*slip.DocArg{
				{
					Name: "arg",
					Type: "list|cons",
					Text: "The value to take the fourth element of.",
				},
			},
			Return: "object",
			Text:   `__fourth__ returns the fourth element in a _list_ or _nil_ if there is no fourth element.`,
			Examples: []string{
				"(fourth nil) => nil",
				"(fourth '(a b c . d) => d",
				"(fourth '(a b c d)) => d",
			},
		}, &slip.CLPkg)
}

// Fourth represents the fourth function.
type Fourth struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Fourth) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	return callN(f, args, 3)
}

// Place a value in the fourth position of a list or cons.
func (f *Fourth) Place(s *slip.Scope, args slip.List, value slip.Object) {
	placeN(f, args, 3, value)
}
