// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Nbutlast{Butlast: Butlast{Function: slip.Function{Name: "nbutlast", Args: args}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "nbutlast",
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
			Text:   `__nbutlast__ returns a copy of the _list_ with the last _n_ elements omitted.`,
			Examples: []string{
				"(nbutlast nil) => nil",
				"(nbutlast '(a b)) => a",
				"(nbutlast '(a b . c)) => (a)",
				"(nbutlast '(a b c d e f) 3) => (a b c)",
			},
		}, &slip.CLPkg)
}

// Nbutlast represents the nbutlast function.
type Nbutlast struct {
	Butlast
}
