// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Seventh{Function: slip.Function{Name: "seventh", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "seventh",
			Args: []*slip.DocArg{
				{
					Name: "list",
					Type: "list|cons",
					Text: "The value to take the seventh element of.",
				},
			},
			Return: "object",
			Text:   `__seventh__ returns the seventh element in a _list_ or _nil_ if there is no seventh element.`,
			Examples: []string{
				"(seventh nil) => nil",
				"(seventh '(a b c d e f . g) => g",
				"(seventh '(a b c d e f g)) => g",
			},
		}, &slip.CLPkg)
}

// Seventh represents the seventh function.
type Seventh struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Seventh) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	return callN(f, args, 6)
}

// Place a value in the seventh position of a list or cons.
func (f *Seventh) Place(s *slip.Scope, args slip.List, value slip.Object) {
	placeN(f, args, 6, value)
}
