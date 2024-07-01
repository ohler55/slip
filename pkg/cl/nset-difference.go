// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := NsetDifference{
				SetDifference: SetDifference{Function: slip.Function{Name: "nset-difference", Args: args}},
			}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "nset-difference",
			Args: []*slip.DocArg{
				{
					Name: "list-1",
					Type: "list",
				},
				{
					Name: "list-2",
					Type: "list",
				},
				{Name: "&key"},
				{
					Name: "key",
					Type: "symbol|lambda",
					Text: `A function that expects one argument to apply to each element
in the lists to return a key for comparison..`,
				},
				{
					Name: "test",
					Type: "symbol|lambda",
					Text: `A function that expects two arguments; the _item_ and each element
in the list at _place_. A return of true indicates a match.`,
				},
			},
			Return: "sequence",
			Text:   `__nset-difference__ returns a list of all element from _list-1_ not in _list-2_.`,
			Examples: []string{
				"(nset-difference '(a b c) '(b d)) => (a c)",
			},
		}, &slip.CLPkg)
}

// NsetDifference represents the nset-difference function.
type NsetDifference struct {
	SetDifference
}
