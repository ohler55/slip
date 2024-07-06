// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Pairlis{Function: slip.Function{Name: "pairlis", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "pairlis",
			Args: []*slip.DocArg{
				{
					Name: "keys",
					Type: "list",
					Text: `Keys for association list elements to be created.`,
				},
				{
					Name: "value",
					Type: "object",
					Text: `Values for association list elements to be created.`,
				},
				{Name: "&optional"},
				{
					Name: "alist",
					Type: "association list",
					Text: "The list to prepend values to.",
				},
			},
			Return: "list",
			Text: `__pairlis__ returns an association list with the pairs of elements from
_keys_ and _values_ as the members. If _alist_ is provided the new members are prepended
to _alist_.`,
			Examples: []string{
				`(pairlis '(a b) '(0 1) '((c . 2))) => ((a . 0) (b . 1) (c . 2))`,
			},
		}, &slip.CLPkg)
}

// Pairlis represents the pairlis function.
type Pairlis struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Pairlis) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 2, 3)
	keys, ok := args[0].(slip.List)
	if !ok {
		slip.PanicType("keys", args[0], "list")
	}
	var values slip.List
	if values, ok = args[1].(slip.List); !ok {
		slip.PanicType("values", args[1], "list")
	}
	if len(keys) != len(values) {
		slip.NewPanic("Lists of keys and values are of unequal length.")
	}
	alist := make(slip.List, len(keys))
	for i, key := range keys {
		alist[i] = slip.List{key, slip.Tail{Value: values[i]}}
	}
	if 2 < len(args) {
		var tail slip.List
		if tail, ok = args[2].(slip.List); !ok {
			slip.PanicType("alist", args[2], "list")
		}
		alist = append(alist, tail...)
	}
	return alist
}
