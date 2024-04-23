// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import "github.com/ohler55/slip"

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Vector{Function: slip.Function{Name: "vector", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "vector",
			Args: []*slip.DocArg{
				{Name: "&rest"},
				{Name: "objects", Type: "object"},
			},
			Return: "vector",
			Text:   `__vector__ returns a _vector_ of all the _objects_.`,
			Examples: []string{
				"(vector) => #()",
				"(vector a b) => #(a b)",
			},
		}, &slip.CLPkg)
}

// Vector represents the vector function.
type Vector struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Vector) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	return slip.NewVector(args, slip.TrueSymbol, true)
}
