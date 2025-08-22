// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := ListLength{Function: slip.Function{Name: "list-length", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "list-length",
			Args: []*slip.DocArg{
				{
					Name: "list",
					Type: "list",
					Text: "The list to return the length of.",
				},
			},
			Return: "fixnum",
			Text:   `__list-length__ returns the length of the _list_.`,
			Examples: []string{
				"(list-length '(a b c)) => 3",
			},
		}, &slip.CLPkg)
}

// ListLength represents the list-length function.
type ListLength struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *ListLength) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	var size slip.Fixnum
	switch ta := args[0].(type) {
	case nil:
		// leave at zero
	case slip.List:
		size = slip.Fixnum(len(ta))
	default:
		slip.TypePanic(s, depth, "list", args[0], "list")
	}
	return size
}
