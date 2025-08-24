// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Tenth{Function: slip.Function{Name: "tenth", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "tenth",
			Args: []*slip.DocArg{
				{
					Name: "list",
					Type: "list|cons",
					Text: "The value to take the tenth element of.",
				},
			},
			Return: "object",
			Text:   `__tenth__ returns the tenth element in a _list_ or _nil_ if there is no tenth element.`,
			Examples: []string{
				"(tenth nil) => nil",
				"(tenth '(a b c d e f g h i . j) => j",
				"(tenth '(a b c d e f g h i j)) => j",
			},
		}, &slip.CLPkg)
}

// Tenth represents the tenth function.
type Tenth struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Tenth) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	return callN(s, f, args, 9, depth)
}

// Place a value in the tenth position of a list or cons.
func (f *Tenth) Place(s *slip.Scope, args slip.List, value slip.Object) {
	placeN(s, f, args, 9, value)
}
