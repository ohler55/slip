// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Nthcdr{Function: slip.Function{Name: "nthcdr", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "nthcdr",
			Args: []*slip.DocArg{
				{
					Name: "n",
					Type: "integer",
					Text: "The index into the list.",
				},
				{
					Name: "list",
					Type: "list",
					Text: "The value to return the _nthcdr_ element of.",
				},
			},
			Return: "object",
			Text:   `__nthcdr__ returns the tail of _list_ as if __cdr__ was called _n_ times.`,
			Examples: []string{
				"(nthcdr 0 '()) => nil",
				"(nthcdr 1 '(a b c)) => (b c)",
			},
		}, &slip.CLPkg)
}

// Nthcdr represents the nthcdr function.
type Nthcdr struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Nthcdr) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 2, 2)
	switch list := args[1].(type) {
	case nil:
	case slip.List:
		num, ok := args[0].(slip.Integer)
		if !ok {
			slip.PanicType("n", args[0], "integer")
		}
		n := int(num.Int64())
		if len(list) <= n || n < 0 {
			return nil
		}
		if len(list)-1 == n {
			if tail, ok := list[n].(slip.Tail); ok {
				return tail.Value
			}
		}
		return list[n:]
	default:
		slip.PanicType("list", args[1], "list")
	}
	return nil
}
