// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Append{Function: slip.Function{Name: "append", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "append",
			Args: []*slip.DocArg{
				{Name: "&rest"},
				{
					Name: "lists",
					Type: "list",
					Text: "The lists to concatenate.",
				},
			},
			Return: "list",
			Text:   `__append__ concatenates copies of the _lists_.`,
			Examples: []string{
				"(append '(a b) '(c d)) => (a b c d)",
				"(append) => nil",
			},
		}, &slip.CLPkg)
}

// Append represents the append function.
type Append struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Append) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	for _, a := range args {
		if a == nil {
			continue
		}
		switch tr := result.(type) {
		case nil:
			if list, ok := a.(slip.List); ok && 0 < len(list) {
				l2 := make(slip.List, len(list))
				copy(l2, list)
				a = l2
			}
			result = a
		case slip.List:
			if len(tr) == 0 {
				if list, ok := a.(slip.List); ok && 0 < len(list) {
					l2 := make(slip.List, len(list))
					copy(l2, list)
					a = l2
				}
				result = a
				break
			}
			if _, ok := tr[len(tr)-1].(slip.Tail); ok {
				slip.TypePanic(s, depth, "list", tr, "list")
			}
			switch ta := a.(type) {
			case nil:
				// no change
			case slip.List:
				if 0 < len(ta) {
					tr = append(tr, ta...)
					result = tr
				}
			default:
				tr = append(tr, slip.Tail{Value: ta})
				result = tr
			}
		default:
			slip.TypePanic(s, depth, "list", tr, "list")
		}
	}
	return
}
