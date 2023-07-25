// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Remove{Delete{Function: slip.Function{Name: "remove", Args: args}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "remove",
			Args: []*slip.DocArg{
				{
					Name: "item",
					Type: "object",
					Text: "The value to match against the elements of _sequence_.",
				},
				{
					Name: "sequence",
					Type: "sequence",
					Text: "The sequence to search for _item_ in.",
				},
				{Name: "&key"},
				{
					Name: "from-end",
					Type: "boolean",
					Text: `If true the search is in reverse or or from the end to the start.`,
				},
				{
					Name: "test",
					Type: "symbol|lambda",
					Text: `A function that expects two arguments and returns a boolean to
indicate a match. The default is _equal_`,
				},
				{
					Name: "start",
					Type: "fixnum",
					Text: `The index to the first element in the sequence to check. Defaults to 0`,
				},
				{
					Name: "end",
					Type: "fixnum",
					Text: `The index to the last element in the sequence to check. Defaults to nil,
the length of the _sequence_.`,
				},
				{
					Name: "count",
					Type: "fixnum",
					Text: `The maximum number of element to remove. Default is _nil_ for unlimited.`,
				},
				{
					Name: "key",
					Type: "symbol|lambda",
					Text: `A function that expects one argument to apply to each element
in the _sequence_ to return a key for comparison.`,
				},
			},
			Return: "object",
			Text:   `__remove__ returns a sequence with the matching elements removed.`,
			Examples: []string{
				"(remove 'x '((y . 1) (y. 2) (z . 3)) :key 'car) => ((y . 1) (y. 2))",
			},
		}, &slip.CLPkg)
}

// Remove represents the remove function.
type Remove struct {
	Delete
}
