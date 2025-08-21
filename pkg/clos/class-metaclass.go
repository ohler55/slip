// Copyright (c) 2022, Peter Ohler, All rights reserved.

package clos

import (
	"github.com/ohler55/slip"
)

func defClassMetaclass() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := ClassMetaclass{Function: slip.Function{Name: "class-metaclass", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "class-metaclass",
			Args: []*slip.DocArg{
				{
					Name: "class",
					Type: "class|flavor",
					Text: "The metaclass of the class or flavor.",
				},
			},
			Return: "symbol",
			Text:   `__class-metaclass__ returns the metaclass of a class or flavor.`,
			Examples: []string{
				"(class-metaclass 'vanilla-flavor) => flavor",
			},
		}, &Pkg)
}

// ClassMetaclass represents the class-metaclass function.
type ClassMetaclass struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *ClassMetaclass) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	return classFromArg0(f, s, args, depth).Metaclass()
}
