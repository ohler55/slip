// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Mapcon{Function: slip.Function{Name: "mapcon", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "mapcon",
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
			Text: `__mapcon__ calls _function_ for each _cdr_ in the _lists_ with
an argument from each list and returns a _list_ of the results of each call.`,
			Examples: []string{
				"(mapcon 'list '(1 2 3)) => ((1 2 3) (2 3) (3))",
			},
		}, &slip.CLPkg)
}

// Mapcon represents the mapcon function.
type Mapcon struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Mapcon) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 2, -1)
	pos := 0
	fn := args[pos]
	d2 := depth + 1
	caller := resolveToCaller(s, fn, d2)

	pos++
	list, ok := args[pos].(slip.List)
	if !ok {
		slip.PanicType("lists", args[pos], "list")
	}
	var rlist slip.List
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
		rl, _ := caller.Call(s, ca, d2).(slip.List)
		rlist = append(rlist, rl...)
	}
	return rlist
}
