// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := ValuesList{Function: slip.Function{Name: "values-list", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "values-list",
			Args: []*slip.DocArg{
				{
					Name: "list",
					Type: "list",
					Text: "The value create a _values_ from.",
				},
			},
			Return: "values",
			Text:   `__values-list__ returns a _values_ from the elements in _list_.`,
			Examples: []string{
				"(values-list nil) => ",
				"(values-list '(a b c)) => a, b, c",
			},
		}, &slip.CLPkg)
}

// ValuesList represents the values-list function.
type ValuesList struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *ValuesList) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	a := args[0]
	switch list := a.(type) {
	case nil:
		result = slip.Values{}
	case slip.List:
		result = slip.Values(list)
	default:
		slip.TypePanic(s, depth, "argument to values-list", list, "list")
	}
	return
}
