// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Eighth{Function: slip.Function{Name: "eighth", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "eighth",
			Args: []*slip.DocArg{
				{
					Name: "list",
					Type: "list|cons",
					Text: "The value to take the eighth element of.",
				},
			},
			Return: "object",
			Text:   `__eighth__ returns the eighth element in a _list_ or _nil_ if there is no eighth element.`,
			Examples: []string{
				"(eighth nil) => nil",
				"(eighth '(a b c d e f g . h) => h",
				"(eighth '(a b c d e f g h)) => h",
			},
		}, &slip.CLPkg)
}

// Eighth represents the eighth function.
type Eighth struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Eighth) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	return callN(s, f, args, 7, depth)
}

// Place a value in the eighth position of a list or cons.
func (f *Eighth) Place(s *slip.Scope, args slip.List, value slip.Object) {
	placeN(s, f, args, 7, value)
}
