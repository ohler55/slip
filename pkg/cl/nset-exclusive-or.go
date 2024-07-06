// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := NsetExclusiveOr{
				SetExclusiveOr: SetExclusiveOr{Function: slip.Function{Name: "nset-exclusive-or", Args: args}},
			}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "nset-exclusive-or",
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
			Text: `__nset-exclusive-or__ returns a list of all element in _list-1_ but
not in _list-2_ and vice-versa.`,
			Examples: []string{
				"(nset-exclusive-or '(a b c) '(b d)) => (a c d)",
			},
		}, &slip.CLPkg)
}

// NsetExclusiveOr represents the nset-exclusive-or function.
type NsetExclusiveOr struct {
	SetExclusiveOr
}
