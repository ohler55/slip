// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Third{Function: slip.Function{Name: "third", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "third",
			Args: []*slip.DocArg{
				{
					Name: "arg",
					Type: "list|cons",
					Text: "The value to take the third element of.",
				},
			},
			Return: "object",
			Text:   `__third__ returns the third element in a _list_ or _nil_ if there is no third element.`,
			Examples: []string{
				"(third nil) => nil",
				"(third '(a b . c) => c",
				"(third '(a b c)) => c",
			},
		}, &slip.CLPkg)
}

// Third represents the third function.
type Third struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Third) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	return callN(s, f, args, 2, depth)
}

// Place a value in the third position of a list or cons.
func (f *Third) Place(s *slip.Scope, args slip.List, value slip.Object) {
	placeN(s, f, args, 2, value)
}
