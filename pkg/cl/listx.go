// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import "github.com/ohler55/slip"

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Listx{Function: slip.Function{Name: "list*", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "list*",
			Args: []*slip.DocArg{
				{Name: "&rest"},
				{Name: "objects", Type: "object"},
			},
			Return: "object",
			Text:   `__list__ returns a _list_ of all the _objects_ with the last element set as the cdr of the list.`,
			Examples: []string{
				"(list* 1) => 1",
				"(list* a b) => (a . b)",
			},
		}, &slip.CLPkg)
}

// Listx represents the list* function.
type Listx struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Listx) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	switch len(args) {
	case 0:
		slip.CheckArgCount(s, depth, f, args, 1, -1)
	case 1:
		result = args[0]
	default:
		list := make(slip.List, len(args))
		copy(list, args)
		list[len(list)-1] = slip.Tail{Value: list[len(list)-1]}
		result = list
	}
	return
}
