// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Mapc{Function: slip.Function{Name: "mapc", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "mapc",
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
			Text: `__mapc__ calls _function_ for each entry in the _lists_ with
an argument from each list.`,
			Examples: []string{
				"(setq list '())",
				"(mapc (lambda (x) (setq list (cons x list))) '(1 2 3)) => (1 2 3)",
				"list => (3 2 1)",
			},
		}, &slip.CLPkg)
}

// Mapc represents the mapc function.
type Mapc struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Mapc) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 2, -1)
	fn := args[0]
	d2 := depth + 1
	caller := ResolveToCaller(s, fn, d2)

	list, ok := args[1].(slip.List)
	if !ok {
		slip.PanicType("lists", args[1], "list")
	}
	if 1 < len(args)-1 {
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
				ca[i-1] = l2[n]
			}
			_ = caller.Call(s, ca, d2)
		}
	} else {
		// The most common case.
		for _, v := range list {
			_ = caller.Call(s, slip.List{v}, d2)
		}
	}
	return list
}
