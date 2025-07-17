// Copyright (c) 2022, Peter Ohler, All rights reserved.

package clos

import (
	"github.com/ohler55/slip"
)

func defClassName() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := ClassName{Function: slip.Function{Name: "class-name", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "class-name",
			Args: []*slip.DocArg{
				{
					Name: "class",
					Type: "class|flavor",
					Text: "The name of the class or flavor.",
				},
			},
			Text: `__class-name__ returns the name of a class or flavor.`,
			Examples: []string{
				"(class-name 'vanilla-flavor) => vanilla-flavor",
			},
		}, &Pkg)
}

// ClassName represents the class-name function.
type ClassName struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *ClassName) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	return slip.Symbol(classFromArg0(f, s, args).Name())
}
