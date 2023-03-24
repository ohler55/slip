// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import "github.com/ohler55/slip"

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := List{Function: slip.Function{Name: "list", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "list",
			Args: []*slip.DocArg{
				{Name: "&rest"},
				{Name: "objects", Type: "object"},
			},
			Return: "list",
			Text:   `__list__ returns a _list_ of all the _objects_.`,
			Examples: []string{
				"(list) => ()",
				"(list a b) => (a b)",
			},
		}, &slip.CLPkg)
}

// List represents the list function.
type List struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *List) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	list := make(slip.List, len(args))
	copy(list, args)
	return list
}
