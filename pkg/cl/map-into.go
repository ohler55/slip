// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := MapInto{Function: slip.Function{Name: "map-into", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "map-into",
			Args: []*slip.DocArg{
				{
					Name: "result-sequence",
					Type: "list",
					Text: "A list to store the results.",
				},
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
			Text: `__map-into__ calls _function_ for each entry in the _lists_ with
an argument from each list and places the result in the _result-sequence_. The _result-sequence_
is returned.`,
			Examples: []string{
				"(let ((seq '(a b c)))",
				" (map-into seq '+ '(1 2 3) '(10 20 30 40))) => (11 22 33)",
			},
		}, &slip.CLPkg)
}

// MapInto represents the map-into function.
type MapInto struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *MapInto) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 2, -1)
	rlist, ok := args[0].(slip.List)
	if !ok {
		slip.PanicType("result-sequence", args[0], "list")
	}
	fn := args[1]
	d2 := depth + 1
	caller := ResolveToCaller(s, fn, d2)
	args = args[2:]
	lists := make([]slip.List, len(args))
	for i, arg := range args {
		var list slip.List
		if list, ok = arg.(slip.List); !ok {
			slip.PanicType("lists", arg, "list")
		}
		lists[i] = list
	}
	ca := make(slip.List, len(args))
	for i := 0; i < len(rlist); i++ {
		for j, list := range lists {
			if len(list) <= i {
				return rlist
			}
			ca[j] = list[i]
		}
		rlist[i] = caller.Call(s, ca, d2)
	}
	return rlist
}
