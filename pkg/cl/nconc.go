// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Nconc{Function: slip.Function{Name: "nconc", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "nconc",
			Args: []*slip.DocArg{
				{Name: "&rest"},
				{
					Name: "lists",
					Type: "list",
					Text: "The lists to concatenate.",
				},
			},
			Return: "object",
			Text:   `__nconc__ concatenates the _lists_ modifying the first list to includes the rest.`,
			Examples: []string{
				"(nconc '(a b) '(c d)) => (a b c d)",
				"(nconc) => nil",
			},
		}, &slip.CLPkg)
}

// Nconc represents the nconc function.
type Nconc struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *Nconc) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	for _, a := range args {
		switch tr := result.(type) {
		case nil:
			result = a
		case slip.List:
			if len(tr) == 0 {
				result = a
				break
			}
			if _, ok := tr[len(tr)-1].(slip.Tail); ok {
				if list, ok := a.(slip.List); ok {
					if len(list) == 0 {
						break
					}
					result = append(tr[:len(tr)-1], list...)
					break
				}
				tr[len(tr)-1] = slip.Tail{Value: a}
				break
			}
			if list, ok := a.(slip.List); ok {
				result = append(tr, list...)
				break
			}
			slip.PanicType("list", a, "list")
		default:
			slip.PanicType("list", a, "list")
		}
	}
	return
}
