// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Nreconc{Function: slip.Function{Name: "nreconc", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "nreconc",
			Args: []*slip.DocArg{
				{
					Name: "list",
					Type: "list",
					Text: "The list to reverse.",
				},
				{
					Name: "tail",
					Type: "list",
					Text: "The list to append to _list_.",
				},
			},
			Return: "list",
			Text: `__nreconc__ reverses the elements in _list_ and appends _tail_ to that list.
The _list_ is modified.`,
			Examples: []string{
				"(nreconc '(a b) '(c d)) => (b a c d)",
			},
		}, &slip.CLPkg)
}

// Nreconc represents the nreconc function.
type Nreconc struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Nreconc) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 2, 2)
	list, ok := args[0].(slip.List)
	if !ok {
		slip.PanicType("list", args[0], "list")
	}
	if 0 < len(list) {
		max := len(list) - 1
		for i := max / 2; 0 <= i; i-- {
			list[i], list[max-i] = list[max-i], list[i]
		}
	}
	switch ta := args[1].(type) {
	case slip.List:
		list = append(list, ta...)
	default:
		list = append(list, slip.Tail{Value: ta})
	}
	return list
}
