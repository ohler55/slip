// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := RemoveDuplicates{DeleteDuplicates{Function: slip.Function{Name: "remove-duplicates", Args: args}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "remove-duplicates",
			Args: []*slip.DocArg{
				{
					Name: "sequence",
					Type: "sequence",
					Text: "The sequence to remove duplicates from.",
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
					Name: "key",
					Type: "symbol|lambda",
					Text: `A function that expects one argument to apply to each element
in the _sequence_ to return a key for comparison.`,
				},
			},
			Return: "object",
			Text:   `__remove-duplicates__ returns a sequence with duplicate elements removed.`,
			Examples: []string{
				"(remove-duplicates '(a b c b d a) => (c b d a)",
			},
		}, &slip.CLPkg)
}

// RemoveDuplicates represents the remove-duplicates function.
type RemoveDuplicates struct {
	DeleteDuplicates
}
