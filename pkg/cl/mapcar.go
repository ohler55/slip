// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Mapcar{Function: slip.Function{Name: "mapcar", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "mapcar",
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
			Text: `__mapcar__ calls _function_ for each entry in the _lists_ with
an argument from each list and returns a _list_ of the results of each call.`,
			Examples: []string{
				"(mapcar (lambda (x) (1+ x)) '(1 2 3)) => (2 3 4)",
			},
		}, &slip.CLPkg)
}

// Mapcar represents the mapcar function.
type Mapcar struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Mapcar) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
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
	var rlist slip.List
	if pos < len(args)-1 {
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
		rlist = make(slip.List, min)
		ca := make(slip.List, len(args)-1)
		for n := 0; n < min; n++ {
			for i := 1; i < len(args); i++ {
				l2 := args[i].(slip.List)
				ca[i-1] = l2[n]
			}
			rlist[n] = caller.Call(s, ca, d2)
		}
	} else {
		// The most common case.
		rlist = make(slip.List, len(list))
		for i, v := range list {
			rlist[i] = caller.Call(s, slip.List{v}, d2)
		}
	}
	return rlist
}
