// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Butlast{Function: slip.Function{Name: "butlast", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "butlast",
			Args: []*slip.DocArg{
				{
					Name: "list",
					Type: "list",
					Text: "The list to take all but the last from.",
				},
				{Name: slip.AmpOptional},
				{
					Name: "n",
					Type: "integer",
					Text: "The number of items to omit when returning a list copy.",
				},
			},
			Return: "list",
			Text:   `__butlast__ returns a copy of the _list_ with the last _n_ elements omitted.`,
			Examples: []string{
				"(butlast nil) => nil",
				"(butlast '(a b)) => a",
				"(butlast '(a b . c)) => (a)",
				"(butlast '(a b c d e f) 3) => (a b c)",
			},
		}, &slip.CLPkg)
}

// Butlast represents the butlast function.
type Butlast struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Butlast) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 1, 2)
	a := args[0]
	switch list := a.(type) {
	case nil:
		// leave result as nil
	case slip.List:
		n := 1
		if 1 < len(args) {
			if i, ok := args[1].(slip.Integer); ok {
				n = int(i.Int64())
			} else {
				slip.TypePanic(s, depth, "n", args[1], "integer")
			}
		}
		if 0 < len(list) {
			if _, ok := list[len(list)-1].(slip.Tail); ok {
				n++
			}
			if 0 <= n && n < len(list) {
				size := len(list) - n
				rlist := make(slip.List, size)
				copy(rlist, list[:size])
				result = rlist
			}
		}
	default:
		slip.TypePanic(s, depth, "list", list, "list")
	}
	return
}
