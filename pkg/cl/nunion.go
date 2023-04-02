// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Nunion{Union: Union{Function: slip.Function{Name: "nunion", Args: args}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "nunion",
			Args: []*slip.DocArg{
				{
					Name: "list-1",
					Type: "list",
					Text: `The first _list_ for the nunion.`,
				},
				{
					Name: "list-2",
					Type: "list",
					Text: `The second _list_ for the nunion.`,
				},
				{Name: "&key"},
				{
					Name: "test",
					Type: "symbol|lambda",
					Text: `A function that expects two arguments and returns _t_ to
indicate the two argument are equal. The default is __equal__.`,
				},
				{
					Name: "key",
					Type: "symbol|lambda",
					Text: `A function that expects one argument to apply to each element
in the _list_ to return a key for comparison.`,
				},
			},
			Return: "list",
			Text:   `__nunion__ returns a list of all the unique elements of both list. Order is arbitrary.`,
			Examples: []string{
				`(nunion '(a b c b a) '(d a b) => (a c b d)`,
			},
		}, &slip.CLPkg)
}

// Nunion represents the nunion function.
type Nunion struct {
	Union
}
