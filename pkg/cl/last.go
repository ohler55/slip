// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Last{Function: slip.Function{Name: "last", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "last",
			Args: []*slip.DocArg{
				{
					Name: "list",
					Type: "list",
					Text: "The list to take all the last from.",
				},
				{Name: slip.AmpOptional},
				{
					Name: "n",
					Type: "integer",
					Text: "The number of items to omit when returning a list copy.",
				},
			},
			Return: "list",
			Text:   `__last__ returns a copy of the _list_ with the last _n_ elements.`,
			Examples: []string{
				"(last nil) => nil",
				"(last '(a b)) => b",
				"(last '(a b . c)) => c",
				"(last '(a b c d e f) 3) => (d e f)",
			},
		}, &slip.CLPkg)
}

// Last represents the last function.
type Last struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Last) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, 2)
	a := args[0]
	switch list := a.(type) {
	case nil:
		// leave result as nil
	case slip.List:
		n := 1
		if 1 < len(args) {
			if i, ok := args[1].(slip.Integer); ok && 0 <= n {
				n = int(i.Int64())
			} else {
				slip.PanicType("n", args[1], "non-negative integer")
			}
		}
		if len(list) <= n {
			result = list
		} else {
			if tail, ok := list[len(list)-1].(slip.Tail); ok {
				if n == 0 {
					result = tail.Value
				} else {
					n++
					if len(list) <= n {
						result = list
					} else {
						rlist := make(slip.List, n)
						copy(rlist, list[len(list)-n:])
						result = rlist
					}
				}
			} else {
				rlist := make(slip.List, n)
				copy(rlist, list[len(list)-n:])
				result = rlist
			}
		}
	default:
		slip.PanicType("list", list, "list")
	}
	return
}
