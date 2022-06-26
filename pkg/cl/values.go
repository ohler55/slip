// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import "github.com/ohler55/slip"

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Values{Function: slip.Function{Name: "values", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "values",
			Args: []*slip.DocArg{
				{Name: "&rest"},
				{Name: "objects", Type: "object"},
			},
			Return: "values",
			Text: `__values__ returns a multi-value object of all the _objects_.
Use only as a return value for functions that return multiple values.`,
			Examples: []string{
				"(values a b) => a, b",
			},
		}, &slip.CLPkg)
}

// Values represents the values function.
type Values struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *Values) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	return slip.Values(args)
}
