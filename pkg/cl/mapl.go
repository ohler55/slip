// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Mapl{Function: slip.Function{Name: "mapl", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "mapl",
			Args: []*slip.DocArg{
				{
					Name: "function",
					Type: "symbol|lambda",
					Text: "The function to call for each entry in _lists_.",
				},
				{Name: "&rest"},
				{
					Name: "lists",
					Type: "list",
					Text: "The lists to iterate over.",
				},
			},
			Return: "nil",
			Text: `__mapl__ calls _function_ for each _cdr_ in the _lists_ with
an argument from each list and returns a _list_ of the original first list.`,
			Examples: []string{
				"(mapl (lambda (x) (1+ (car x))) '(1 2 3)) => (1 2 3)",
			},
		}, &slip.CLPkg)
}

// Mapl represents the mapl function.
type Mapl struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Mapl) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 2, -1)
	pos := 0
	fn := args[pos]
	d2 := depth + 1
	caller := ResolveToCaller(s, fn, d2)

	pos++
	list, ok := args[pos].(slip.List)
	if !ok {
		slip.PanicType("lists", args[pos], "list")
	}
	min := len(list)
	var l2 slip.List
	for i := 1; i < len(args); i++ {
		if l2, ok = args[i].(slip.List); !ok {
			slip.PanicType("lists", args[i], "list")
		}
		if len(l2) < min {
			min = len(l2)
		}
	}
	ca := make(slip.List, len(args)-1)
	for n := 0; n < min; n++ {
		for i := 1; i < len(args); i++ {
			l2 := args[i].(slip.List)
			ca[i-1] = l2[n:]
		}
		_ = caller.Call(s, ca, d2)
	}
	return list
}
