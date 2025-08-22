// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := First{Function: slip.Function{Name: "first", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "first",
			Args: []*slip.DocArg{
				{
					Name: "arg",
					Type: "list|cons",
					Text: "The value to take the first element of.",
				},
			},
			Return: "object",
			Text:   `__first__ returns the first element in a _list_ or _nil_ if there is no first element.`,
			Examples: []string{
				"(first nil) => nil",
				"(first '(a . b) => a",
				"(first '(a b c)) => a",
			},
		}, &slip.CLPkg)
}

// First represents the first function.
type First struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *First) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	a := args[0]
	switch list := a.(type) {
	case nil:
		// leave result as nil
	case slip.List:
		if 0 < len(list) {
			result = list[0]
		}
	default:
		slip.TypePanic(s, depth, "argument to first", list, "cons", "list")
	}
	return
}

// Place a value in the first position of a list or cons.
func (f *First) Place(s *slip.Scope, args slip.List, value slip.Object) {
	slip.CheckArgCount(s, 0, f, args, 1, 1)
	if list, ok := args[0].(slip.List); ok && 0 < len(list) {
		list[0] = value
		return
	}
	slip.TypePanic(s, 0, "argument to first", args[0], "cons", "list")
}
