// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Second{Function: slip.Function{Name: "second", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "second",
			Args: []*slip.DocArg{
				{
					Name: "arg",
					Type: "list|cons",
					Text: "The value to take the second element of.",
				},
			},
			Return: "object",
			Text:   `__second__ returns the second element in a _list_ or _nil_ if there is no second element.`,
			Examples: []string{
				"(second nil) => nil",
				"(second '(a . b) => b",
				"(second '(a b c)) => b",
			},
		}, &slip.CLPkg)
}

// Second represents the second function.
type Second struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Second) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	return callN(s, f, args, 1, depth)
}

// Place a value in the second position of a list or cons.
func (f *Second) Place(s *slip.Scope, args slip.List, value slip.Object) {
	placeN(s, f, args, 1, value)
}
