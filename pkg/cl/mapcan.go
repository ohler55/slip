// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Mapcan{Function: slip.Function{Name: "mapcan", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "mapcan",
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
			Text: `__mapcan__ calls _function_ for each entry in the _lists_ with
an argument from each list and returns a _list_ of the results of each _nconc_ed together.`,
			Examples: []string{
				"(mapcan (lambda (x y) (list x y)) '(a b c) '(1 2 3)) => (a 1 b 2 c 3)",
			},
		}, &slip.CLPkg)
}

// Mapcan represents the mapcan function.
type Mapcan struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Mapcan) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 2, -1)
	pos := len(args) - 1
	fn := args[pos]
	d2 := depth + 1
	caller := resolveToCaller(s, fn, d2)

	pos--
	list, ok := args[pos].(slip.List)
	if !ok {
		slip.PanicType("lists", args[pos], "list")
	}
	var rlist slip.List
	min := len(list)
	var l2 slip.List
	for i := 0; i < pos; i++ {
		if l2, ok = args[i].(slip.List); !ok {
			slip.PanicType("lists", args[i], "list")
		}
		if len(l2) < min {
			min = len(l2)
		}
	}
	ca := make(slip.List, pos+1)
	for n := 1; n <= min; n++ {
		for i := 0; i <= pos; i++ {
			l2 := args[i].(slip.List)
			ca[i] = l2[len(l2)-n]
		}
		r := caller.Call(s, ca, d2)
		switch tr := r.(type) {
		case nil:
			// ok but nothing to append
		case slip.List:
			rlist = append(tr, rlist...)
		case slip.Cons:
			// TBD how to handle cons as last one?
			rlist = append(slip.List{tr.Car()}, rlist...)
		default:
			slip.PanicType("list item", tr, "list", "nil", "cons")
		}
	}
	return rlist
}
