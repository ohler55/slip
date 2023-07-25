// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Sixth{Function: slip.Function{Name: "sixth", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "sixth",
			Args: []*slip.DocArg{
				{
					Name: "list",
					Type: "list|cons",
					Text: "The value to take the sixth element of.",
				},
			},
			Return: "object",
			Text:   `__sixth__ returns the sixth element in a _list_ or _nil_ if there is no sixth element.`,
			Examples: []string{
				"(sixth nil) => nil",
				"(sixth '(a b c d e . f) => f",
				"(sixth '(a b c d e f)) => f",
			},
		}, &slip.CLPkg)
}

// Sixth represents the sixth function.
type Sixth struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Sixth) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	return callN(f, args, 5)
}

// Place a value in the sixth position of a list or cons.
func (f *Sixth) Place(args slip.List, value slip.Object) {
	placeN(f, args, 5, value)
}
